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

type EnumInfo = { Name: ComplexName; Symbols: string list }
type FieldInfo = { Name: string;  Type: Type; IsKey: bool }
type RecordInfo = { Name: ComplexName;  Fields: FieldInfo list }
type UnionInfo = { Name: ComplexName;  Cases: RecordInfo list }

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

type LockCache = Map<ComplexName,LockItem>
type TypesCache = Map<ComplexName,ModuleItem>

type Command = {
    Name: string
    Description: string
    Run: Module -> LockItem list -> TypesCache -> string list -> Result<unit,string>
}

module rec Types =

    open Utils

    type TypeError =
        | UnresolvedType of Type
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
        | AddingFieldIsNotAllowed of recordName:ComplexName*fieldName:string

    let (|EmptyCase|SingleParamCase|MultiParamCase|) = function
        | [] -> EmptyCase
        | [Field {Type = Optional (_)}] -> MultiParamCase
        | [Field fieldLock] -> SingleParamCase fieldLock
        | _ -> MultiParamCase

    let mergeName (ComplexName ns) name = ComplexName (name::ns)

    let firstName = function
        | (ComplexName (name::ns)) -> name
        | _ -> failwith "empty name is not supposed"

    let extractNamespace (ComplexName ns) =
        match ns with
        | _::tail -> ComplexName (tail)
        | [] -> ComplexName []

    let moduleItemName = function
        | Enum x -> x.Name
        | Record x -> x.Name
        | Union x -> x.Name

    let lockItemName = function
        | EnumLock lock -> lock.Name
        | MessageLock lock -> lock.Name

    let messageLockItemName = function
        | Field x -> x.Name
        | OneOf (name, _, _) -> name

    let lockItemMaxNum = function
        | Field x -> x.Num
        | OneOf (_, _, cases) -> (cases |> List.maxBy (fun c -> c.Num)).Num

    let (|IsEnum|_|) (typesCache:TypesCache) type' =
        match type' with
        | Complex typeName ->
            match typesCache.TryFind typeName with
            | Some (Enum info) -> Some (info)
            | _ -> None
        | _ -> None

    let (|IsRecord|_|) (typesCache:TypesCache) type' =
        match type' with
        | Complex typeName ->
            match typesCache.TryFind typeName with
            | Some (Record info) -> Some (info)
            | _ -> None
        | _ -> None

    let (|IsUnion|_|) (typesCache:TypesCache) type' =
        match type' with
        | Complex typeName ->
            match typesCache.TryFind typeName with
            | Some (Union info) -> Some (info)
            | _ -> None
        | _ -> None

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

    let toTypesCacheItems (module': Module) =
        module'.Items
        |> List.map (fun item -> moduleItemName item, item)

    let resolveReferences (module': Module): Result<Module, TypeError list> =
        let typesCache = module' |> toTypesCacheItems |> Map.ofList

        let (ComplexName ns) = module'.Name
        let getFullName typeName =
            let (ComplexName name') = typeName
            let rec f ns =
                let testName = ComplexName (name' @ ns)
                if typesCache.ContainsKey(ComplexName (name' @ ns)) then Some testName
                else
                    match ns with
                    | _::tail -> f tail
                    | [] -> None
            f ns

        let resolveRecord info =
            info.Fields
            |> traverse (fun fieldInfo ->
                match fieldInfo.Type with
                | Optional (Complex typeName) -> getFullName typeName |> Option.map (Complex >> Optional)
                | Array (Complex typeName) -> getFullName typeName |> Option.map (Complex >> Array)
                | Map (Complex typeName) -> getFullName typeName |> Option.map (Complex >> Map)
                | Complex typeName  -> getFullName typeName |> Option.map Complex
                | t -> Some t
                |> Option.map (fun type' -> Ok {fieldInfo with Type = type'})
                |> Option.defaultWith (fun () -> Error [UnresolvedType fieldInfo.Type]) )
            |> Result.map (fun fields -> {info with Fields = fields})

        module'.Items
        |> traverse (fun item ->
            match item with
            | Enum _ -> Ok item
            | Record info -> resolveRecord info |> Result.map Record
            | Union info ->
                info.Cases
                |> traverse resolveRecord
                |> Result.map (fun cases -> Union {info with Cases = cases}) )
        |> Result.map (fun items -> {module' with Items = items})

    let lock (module': Module) (currentLock: LockItem list) (typesCache:TypesCache) : Result<LockItem list, TypeError list> =
        currentLock
        |> tryMap lockItemName id
        |> Result.mapError(List.map DuplicateLockedTypeNames)
        |> Result.bind(fun lockCache ->
            module'.Items
            |> traverse (function
                | Enum info ->
                    lockEnum lockCache info
                    |> Result.map List.singleton
                | Record info ->
                    lockRecord lockCache typesCache false info
                    |> Result.map List.singleton
                | Union info ->
                    info.Cases
                    |> traverse (lockRecord lockCache typesCache true) )
            |> Result.map List.concat )

    let lockEnum (lockCache:LockCache) info =
        match lockCache.TryFind info.Name with
        | Some (EnumLock lock) -> Ok lock.Values
        | Some _ -> [DifferenTypeIsLockedWithThatName info.Name] |> Error
        | None -> Ok [] // new enum
        |> Result.bind(fun valuesLock ->
            valuesLock
            |> tryMap (fun x -> x.Name) id
            |> Result.mapError (List.map (fun symbol -> DuplicateSymbolsInLockedEnum (info.Name,symbol)))
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
                    (fun symbol -> DuplicateSymbolsInEnum (info.Name,symbol))
                    (fun symbol -> MissedSymbolInEnum (info.Name, symbol))
                    valuesLock
                |> Result.map (fun values -> EnumLock {Name = info.Name; Values = values})))

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

    let lockRecord (lockCache:LockCache) (typesCache:TypesCache) fieldsAreLocked info : Result<LockItem, TypeError list> =
        match lockCache.TryFind info.Name with
        | Some (MessageLock lock) -> Ok lock.LockItems
        | Some _ -> [DifferenTypeIsLockedWithThatName info.Name] |> Error
        | None -> Ok [] // new record
        |> Result.bind(fun messageLockItems ->
            messageLockItems
            |> tryMap messageLockItemName id
            |> Result.mapError (List.map (fun fieldName -> DuplicateFieldInLockedMessage (info.Name,fieldName)))
            |> Result.bind (fun fieldsMap ->
                let maxNum = messageLockItems |> List.fold (fun m -> lockItemMaxNum >>  max m) 0

                info.Fields
                |> List.mapFold(fun nextNum field ->
                    match field.Type with
                    | IsUnion typesCache unionInfo ->
                        match lockUnion info.Name fieldsMap field.Name unionInfo nextNum with
                        | Ok (locks,nextNum) -> Ok (OneOf (field.Name, unionInfo.Name, locks)), nextNum
                        | Error err -> Error err, nextNum
                    | _ -> // regular field
                        match fieldsMap.TryFind field.Name with
                        | None -> // new field
                            if not fieldsMap.IsEmpty && fieldsAreLocked then
                                Error [AddingFieldIsNotAllowed (info.Name, field.Name)], nextNum
                            else
                                Ok (Field {Name = field.Name; Type = field.Type; Num = nextNum}), (nextNum + 1)
                        | Some item ->
                            match item with
                            | Field lock when allowedEvolution lock.Type field.Type ->
                                Ok (Field {Name = field.Name; Type = field.Type; Num = lock.Num}), nextNum
                            | Field lock ->
                                Error [UnacceptableEvolution(info.Name, field.Name, lock.Type, field.Type)], nextNum
                            | OneOf (_, lockedUnionName, _) ->
                                Error [UnacceptableEvolution(info.Name, field.Name, Complex lockedUnionName, field.Type)], nextNum)
                    (maxNum + 1)
                |> fst
                |> traverse id
                |> Result.bind(
                    checkMissedItems
                        messageLockItemName
                        (fun fieldName -> DuplicateFieldInRecord (info.Name,fieldName))
                        (fun fieldName -> MissedFieldInRecord (info.Name, fieldName))
                        messageLockItems)
                |> Result.map (fun items -> MessageLock {Name = info.Name; LockItems = items})))

    let lockUnion recordName fieldsMap fieldName info nextNum =
        match fieldsMap.TryFind fieldName with
        | Some (OneOf (_, lockedUnionName, lockedCases)) when lockedUnionName = info.Name -> Ok lockedCases
        | Some (OneOf (_, lockedUnionName, _)) -> Error [UnionNameIsChanged(recordName, fieldName, lockedUnionName, info.Name)]
        | Some (Field lock) -> Error [UnacceptableEvolution(recordName, fieldName, lock.Type, Complex info.Name)]
        | None -> Ok []
        |> Result.bind(fun lockedCases ->
            lockedCases
            |> tryMap (fun c -> c.CaseName) id
            |> Result.mapError (List.map (fun fieldName -> DuplicateFieldInLockedMessage (recordName,fieldName)))
            |> Result.bind (fun lockedCasesMap ->
                let newLockedCases =
                    info.Cases
                    |> List.mapFold (fun nextNum case ->
                        match lockedCasesMap.TryFind(firstName case.Name) with
                        | Some lockedCase -> (lockedCase, nextNum)
                        | None -> ({CaseName = firstName case.Name; Num = nextNum}, (nextNum + 1)))
                        nextNum

                fst newLockedCases
                |> checkMissedItems
                    (fun c -> c.CaseName)
                    (fun fieldName -> DuplicateFieldInRecord (recordName,fieldName))
                    (fun fieldName -> MissedCaseInUnion (recordName, info.Name, fieldName))
                    lockedCases
                |> Result.map (fun cases -> cases, (snd newLockedCases))))
