module Protogen.Types

type ComplexName =
    ComplexName of string list

type Type =
    | Bool
    | String
    | Int
    | Long
    | Decimal of scale: int
    | Float
    | Double
    | Bytes
    | Timestamp
    | Duration
    | Guid
    | Optional of value: Type
    | Array of value: Type
    | Map of value: Type
    | Complex of name: ComplexName

type EnumInfo = { Name: string; Symbols: string list }
type FieldInfo = { Name: string;  Type: Type }
type RecordInfo = { Name: string;  Fields: FieldInfo list }
type UnionInfo = { Name: string;  Cases: RecordInfo list }

type ModuleItem =
    | Enum of EnumInfo
    | Record of RecordInfo
    | Union of UnionInfo

type Module = {
    Name: ComplexName
    Items: ModuleItem list
}

type EnumValueLock = { Name: string; Num: int }
type EnumLock = { Name: ComplexName; Values: EnumValueLock list }
type MessageFieldLock = { Name: string;  Type: Type; Num: int }
type OneOfFieldLock = { CaseName: string;  Num: int }
type MessageLockItem =
    | Field of MessageFieldLock
    | OneOf of name:string*unionName:ComplexName*fields:OneOfFieldLock list
type MessageLock = { Name: ComplexName; LockItems: MessageLockItem list }

type LockItem =
    | EnumLock of EnumLock
    | MessageLock of MessageLock

type Command = {
    Name: string
    Description: string
    Run: Module list -> LockItem list -> string list -> Result<unit,string>
}

module rec Types =

    open Utils

    type LockError =
        | General of string
        | DuplicateLockedTypeNames of ComplexName
        | DuplicateTypeNames of ComplexName
        | DifferenTypeIsLockedWithThatName of ComplexName
        | DuplicateSymbolsInLockedEnum of enumName:ComplexName*symbol:string
        | DuplicateSymbolsInEnum of enumName:ComplexName*symbol:string
        | MissedSymbolInEnum of enumName:ComplexName*symbol:string
        | UnknownFieldType of recordName:ComplexName*fieldName:string*typeName:ComplexName
        | DuplicateFieldInLockedMessage of enumName:ComplexName*fieldName:string
        | MissedFieldInRecord of recordName:ComplexName*fieldName:string
        | DuplicateFieldInRecord of recordName:ComplexName*fieldName:string
        | UnionNameIsChanged of recordName:ComplexName*fieldName:string*oldName:ComplexName*newName:ComplexName
        | UnionChangedToRecord of recordName:ComplexName*fieldName:string*unionName:string
        | UnacceptableEvolution of recordName:ComplexName*fieldName:string*oldType:Type*newType:Type
        | MissedCaseInUnion of recordName:ComplexName*unionName:ComplexName*fieldName:string

    type LockCache = Map<ComplexName,LockItem>
    type TypesCache = Map<ComplexName,ModuleItem>

    let mergeName (ComplexName ns) name = ComplexName (name::ns)

    let extractNamespace (ComplexName ns) =
        match ns with
        | _::tail -> ComplexName (tail)
        | [] -> ComplexName []


    let moduleItemName ns = function
        | Enum x -> mergeName ns x.Name
        | Record x -> mergeName ns x.Name
        | Union x -> mergeName ns x.Name

    let lockItemName = function
        | EnumLock lock -> lock.Name
        | MessageLock lock -> lock.Name

    let messageLockItemName = function
        | Field x -> x.Name
        | OneOf (name, _, _) -> name

    let lockItemMaxNum = function
        | Field x -> x.Num
        | OneOf (_, _, cases) -> (cases |> List.maxBy (fun c -> c.Num)).Num

    let lock (modules: Module list) (currentLock: LockItem list) : Result<LockItem list, LockError list> =

        currentLock
        |> tryMap lockItemName id
        |> Result.mapError(List.map DuplicateLockedTypeNames)
        |> Result.bind(fun lockCache ->
            modules
            |> List.collect (fun x -> x.Items |> List.map (fun i -> (moduleItemName x.Name i), i))
            |> tryMap fst snd
            |> Result.mapError(List.map DuplicateTypeNames)
            |> Result.bind(fun typesCache ->
                let lockModule (module':Module) : Result<LockItem list, LockError list> =
                    module'.Items
                    |> traverse (function
                        | Enum info -> lockEnum lockCache module'.Name info |> (Result.map List.singleton)
                        | Record info -> lockRecord lockCache typesCache module'.Name info |> (Result.map List.singleton)
                        | Union info ->
                            info.Cases
                            |> traverse (lockRecord lockCache typesCache (mergeName module'.Name info.Name)))
                    |> Result.map List.concat

                modules
                |> traverse lockModule
                |> Result.map List.concat ))

    let lockEnum (lockCache:LockCache) ns info =

        let fullName = mergeName ns info.Name

        match lockCache.TryFind fullName with
        | Some (EnumLock lock) -> Ok lock.Values
        | Some _ -> [DifferenTypeIsLockedWithThatName fullName] |> Error
        | None -> Ok [] // new enum
        |> Result.bind(fun valuesLock ->
            valuesLock
            |> tryMap (fun x -> x.Name) id
            |> Result.mapError (List.map (fun symbol -> DuplicateSymbolsInLockedEnum (fullName,symbol)))
            |> Result.bind (fun symbolsMap ->
                let maxNum = valuesLock |> List.fold (fun m x -> max m x.Num) 0

                let newValuesLock =
                    info.Symbols
                    |> List.mapFold(fun nextNum symbol ->
                        match symbolsMap.TryFind symbol with
                        | Some lock -> lock,nextNum
                        | None -> {Name = symbol; Num = nextNum}, (nextNum + 1))
                        (maxNum + 1)
                    |> fst

                newValuesLock
                |> checkMissedItems
                    (fun x -> x.Name)
                    (fun symbol -> DuplicateSymbolsInEnum (fullName,symbol))
                    (fun symbol -> MissedSymbolInEnum (fullName, symbol))
                    valuesLock
                |> Result.map (fun values -> EnumLock {Name = fullName; Values = values})))

    let tryFindType (typesCache:TypesCache) (ComplexName(ns)) (ComplexName(typeName)) =
        let rec f ns =
            let testName = ComplexName (typeName @ ns)
            typesCache.TryFind(testName)
            |> Option.map(fun item -> testName,item)
            |> Option.orElseWith(fun () ->
                match ns with
                | [] -> None
                | _::tail -> f tail)
        f ns

    let (|IsUnion|_|) (typesCache:TypesCache) ns type' =
        match type' with
        | Complex typeName ->
            match tryFindType typesCache ns typeName with
            | Some (fullTypeName, Union info) -> Some (fullTypeName,info)
            | _ -> None
        | _ -> None

    let (|IsUnknownType|_|) (typesCache:TypesCache) ns type' =
        match type' with
        | Optional (Complex typeName)
        | Array (Complex typeName)
        | Map (Complex typeName)
        | Complex typeName ->
            match tryFindType typesCache ns typeName with
            | Some _ -> None
            | None -> Some typeName
        | _ -> None


    let toFullQualifiedType (typesCache:TypesCache) (ComplexName ns) type' =

        let getFullName (ComplexName(typeName)) =
            let rec f ns =
                let testName = ComplexName (typeName @ ns)
                if typesCache.ContainsKey(ComplexName (typeName @ ns)) then testName
                else
                    match ns with
                    | _::tail -> f tail
                    | [] -> failwithf "Cann't find type %A" typeName
            f ns

        match type' with
        | Optional (Complex typeName) -> getFullName typeName |> Complex |> Optional
        | Array (Complex typeName) -> getFullName typeName |> Complex |> Array
        | Map (Complex typeName) -> getFullName typeName |> Complex |> Map
        | Complex typeName  -> getFullName typeName |> Complex
        | t -> t

    let allowedEvolution from to' =
        // TODO: allow more evolutions
        match from,to' with
        | Bool, Int -> true
        | Bool, Long -> true
        | Int, Long -> true
        | Int, Bool -> true
        | Long, Int -> true
        | Long, Bool -> true
        | Double, Float -> true
        | Float, Double -> true
        | _ -> from = to'

    let lockRecord (lockCache:LockCache) (typesCache:TypesCache) ns info : Result<LockItem, LockError list> =
        let fullName = mergeName ns info.Name

        match lockCache.TryFind fullName with
        | Some (MessageLock lock) -> Ok lock.LockItems
        | Some _ -> [DifferenTypeIsLockedWithThatName fullName] |> Error
        | None -> Ok [] // new record
        |> Result.bind(fun messageLockItems ->
            messageLockItems
            |> tryMap messageLockItemName id
            |> Result.mapError (List.map (fun fieldName -> DuplicateFieldInLockedMessage (fullName,fieldName)))
            |> Result.bind (fun fieldsMap ->
                let maxNum = messageLockItems |> List.fold (fun m -> lockItemMaxNum >>  max m) 0

                info.Fields
                |> List.mapFold(fun nextNum field ->
                    match field.Type with
                    | IsUnknownType typesCache ns typeName -> Error [UnknownFieldType (fullName,field.Name,typeName)], nextNum
                    | IsUnion typesCache ns (unionName, info) ->
                        match lockUnion fullName fieldsMap field.Name unionName info nextNum with
                        | Ok (locks,nextNum) -> Ok (OneOf (field.Name, unionName, locks)), nextNum
                        | Error err -> Error err, nextNum
                    | _ -> // regular field
                        let fullQualifiedType = toFullQualifiedType typesCache ns field.Type
                        match fieldsMap.TryFind field.Name with
                        | None -> // new field
                            Ok (Field {Name = field.Name; Type = fullQualifiedType; Num = nextNum}), (nextNum + 1)
                        | Some item ->
                            match item with
                            | Field lock when allowedEvolution lock.Type fullQualifiedType ->
                                Ok (Field {Name = field.Name; Type = fullQualifiedType; Num = lock.Num}), nextNum
                            | Field lock ->
                                Error [UnacceptableEvolution(fullName, field.Name, lock.Type, field.Type)], nextNum
                            | OneOf (_, lockedUnionName, _) ->
                                Error [UnacceptableEvolution(fullName, field.Name, Complex lockedUnionName, field.Type)], nextNum)
                    (maxNum + 1)
                |> fst
                |> traverse id
                |> Result.bind(
                    checkMissedItems
                        messageLockItemName
                        (fun fieldName -> DuplicateFieldInRecord (fullName,fieldName))
                        (fun fieldName -> MissedFieldInRecord (fullName, fieldName))
                        messageLockItems)
                |> Result.map (fun items -> MessageLock {Name = fullName; LockItems = items})))

    let lockUnion recordName fieldsMap fieldName unionName info nextNum =
        match fieldsMap.TryFind fieldName with
        | Some (OneOf (_, lockedUnionName, lockedCases)) when lockedUnionName = unionName -> Ok lockedCases
        | Some (OneOf (_, lockedUnionName, _)) -> Error [UnionNameIsChanged(recordName, fieldName, lockedUnionName, unionName)]
        | Some (Field lock) -> Error [UnacceptableEvolution(recordName, fieldName, lock.Type, Complex unionName)]
        | None -> Ok []
        |> Result.bind(fun lockedCases ->
            lockedCases
            |> tryMap (fun c -> c.CaseName) id
            |> Result.mapError (List.map (fun fieldName -> DuplicateFieldInLockedMessage (recordName,fieldName)))
            |> Result.bind (fun lockedCasesMap ->
                let newLockedCases =
                    info.Cases
                    |> List.mapFold (fun nextNum case ->
                        match lockedCasesMap.TryFind(case.Name) with
                        | Some lockedCase -> (lockedCase, nextNum)
                        | None -> ({CaseName = case.Name; Num = nextNum}, (nextNum + 1)))
                        nextNum

                fst newLockedCases
                |> checkMissedItems
                    (fun c -> c.CaseName)
                    (fun fieldName -> DuplicateFieldInRecord (recordName,fieldName))
                    (fun fieldName -> MissedCaseInUnion (recordName, unionName, fieldName))
                    lockedCases
                |> Result.map (fun cases -> cases, (snd newLockedCases))))
