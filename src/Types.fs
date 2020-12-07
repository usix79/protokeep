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
    | Timespamp
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
type MessageOneOfCaseLock = { CaseName: string; Num: int }
type MessageLockItem =
    | Field of MessageFieldLock
    | OneOf of name:string*unionName:ComplexName*fields:MessageOneOfCaseLock list
type MessageLock = { Name: ComplexName; LockItems: MessageLockItem list }

type LockItem =
    | Empty
    | EnumLock of EnumLock
    | MessageLock of MessageLock

module rec Evolution =

    open Utils

    type Error =
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
        | UnacceptableEvolutionOfFieldType of recordName:ComplexName*fieldName:string*oldType:Type*newType:Type
        | MissedCaseInRecord of recordName:ComplexName*unionName:ComplexName*fieldName:string

    type LockCache = Map<ComplexName,LockItem>
    type TypesCache = Map<ComplexName,ModuleItem>

    let lock (modules: Module list) (currentLock: LockItem list) : Result<LockItem list, Error list> =

        currentLock
        |> lockItemsToKeyValuePairs
        |> tryMap
        |> Result.mapError(List.map DuplicateLockedTypeNames)
        |> Result.bind(fun lockCache ->
            modules
            |> List.collect (fun x -> moduleItemsToKeyValuePairs x.Name x.Items)
            |> tryMap
            |> Result.mapError(List.map DuplicateTypeNames)
            |> Result.bind(fun typesCache ->
                let lockModule (module':Module) : Result<LockItem list, Error list> =
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

    let mergeName (ComplexName ns) name = ComplexName (name::ns)

    let lockItemsToKeyValuePairs items =
        [ for item in items do
            match item with
            | EnumLock lock -> lock.Name, item
            | MessageLock lock -> lock.Name, item
            | Empty -> () ]

    let moduleItemsToKeyValuePairs ns items =
        [ for item in items do
            match item with
            | Enum x -> mergeName ns x.Name, item
            | Record x -> mergeName ns x.Name, item
            | Union x ->
                let unionName = mergeName ns x.Name
                unionName, item
                for case in x.Cases do
                    mergeName unionName case.Name, Record case
        ]

    let valueLocksToKeyValuePairs (items: EnumValueLock list) =
        items |> List.map (fun x -> x.Name, x)

    let messageLockItemsToKeyValuePairs (items: MessageLockItem list) =
        items
        |> List.map (fun i ->
            match i with
            | Field x -> x.Name, i
            | OneOf (name, _, _) -> name, i)

    let lockEnum (lockCache:LockCache) ns info =

        let fullName = mergeName ns info.Name

        match lockCache.TryFind fullName with
        | Some (EnumLock lock) -> Ok lock.Values
        | Some _ -> [DifferenTypeIsLockedWithThatName fullName] |> Error
        | None -> Ok [] // new enum
        |> Result.bind(fun valuesLock ->
            valueLocksToKeyValuePairs valuesLock
            |> tryMap
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

                valueLocksToKeyValuePairs newValuesLock
                |> tryMap
                |> Result.mapError (List.map (fun symbol -> DuplicateSymbolsInEnum (fullName,symbol)))
                |> Result.bind (fun newSymbolsMap ->

                    let missedSymbols =
                        valuesLock
                        |> List.choose (fun x ->
                            match newSymbolsMap.TryFind x.Name with
                            | Some _ -> None
                            | None -> Some x.Name)

                    if missedSymbols.IsEmpty then
                        EnumLock {Name = fullName; Values = newValuesLock} |> Ok
                    else
                        missedSymbols
                        |> List.map (fun symbol -> MissedSymbolInEnum (fullName, symbol))
                        |> Error)))

    let lockItemName = function
        | Field x -> x.Name
        | OneOf (name, _, _) -> name

    let lockItemMaxNum = function
        | Field x -> x.Num
        | OneOf (_, _, cases) -> (cases |> List.maxBy (fun c -> c.Num)).Num

    let tryFindType (typesCache:TypesCache) (ComplexName(ns)) (ComplexName(typeName)) =
        let rec f ns =
            typesCache.TryFind(ComplexName (typeName @ ns))
            |> Option.orElseWith(fun () ->
                match ns with
                | [] -> None
                | _::tail -> f tail)
        f ns

    let (|IsUnion|_|) (typesCache:TypesCache) ns type' =
        match type' with
        | Complex typeName ->
            match tryFindType typesCache ns typeName with
            | Some (Union info) -> Some (typeName,info)
            | _ -> None
        | _ -> None

    let (|IsUnknownType|_|) (typesCache:TypesCache) ns type' =
        match type' with
        | Optional (Complex typeName)
        | Array (Complex typeName)
        | Map (Complex typeName)
        | Complex typeName ->
            match tryFindType typesCache ns typeName with
            | Some (Union info) -> None
            | _ -> Some typeName
        | _ -> None

    let allowedEvolution from to' =
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

    let lockRecord (lockCache:LockCache) (typesCache:TypesCache) ns info : Result<LockItem, Error list> =
        let fullName = mergeName ns info.Name

        match lockCache.TryFind fullName with
        | Some (MessageLock lock) -> Ok lock.LockItems
        | Some _ -> [DifferenTypeIsLockedWithThatName fullName] |> Error
        | None -> Ok [] // new record
        |> Result.bind(fun messageLockItems ->
            messageLockItemsToKeyValuePairs messageLockItems
            |> tryMap
            |> Result.mapError (List.map (fun fieldName -> DuplicateFieldInLockedMessage (fullName,fieldName)))
            |> Result.bind (fun fieldsMap ->
                let maxNum = messageLockItems |> List.fold (fun m -> lockItemMaxNum >>  max m) 0

                info.Fields
                |> List.mapFold(fun nextNum field ->
                    match field.Type with
                    | IsUnknownType typesCache ns typeName -> Error [UnknownFieldType (fullName,field.Name,typeName)], nextNum
                    | IsUnion typesCache ns (unionName, info) ->
                        let res =
                            match fieldsMap.TryFind field.Name with
                            | Some (OneOf (_, lockedUnionName, lockedCases)) when lockedUnionName = unionName -> Ok lockedCases
                            | Some (OneOf (_, lockedUnionName, _)) ->
                                Error [UnacceptableEvolutionOfFieldType(fullName, field.Name, Complex lockedUnionName, Complex unionName)]
                            | Some (Field lock) ->
                                Error [UnacceptableEvolutionOfFieldType(fullName, field.Name, lock.Type, field.Type)]
                            | None -> Ok []
                            |> Result.bind(fun lockedCases ->
                                lockedCases
                                |> List.map (fun c -> c.CaseName, c)
                                |> tryMap
                                |> Result.mapError (List.map (fun fieldName -> DuplicateFieldInLockedMessage (fullName,fieldName)))
                                |> Result.bind (fun lockedCasesMap ->
                                    let newLockedCases =
                                        info.Cases
                                        |> List.mapFold (fun nextNum case ->
                                            match lockedCasesMap.TryFind(case.Name) with
                                            | Some lockedCase -> (lockedCase, nextNum)
                                            | None -> ({CaseName = case.Name; Num = nextNum}, (nextNum + 1)))
                                            nextNum

                                    fst newLockedCases
                                    |> List.map (fun c -> c.CaseName, c)
                                    |> tryMap
                                    |> Result.mapError (List.map (fun fieldName -> DuplicateFieldInRecord (fullName,fieldName)))
                                    |> Result.bind (fun newLockedCasesMap ->
                                        let missedCases =
                                            lockedCases
                                            |> List.choose (fun x ->
                                                match newLockedCasesMap.TryFind x.CaseName with
                                                | Some _ -> None
                                                | None -> Some x.CaseName)

                                        if missedCases.IsEmpty then Ok newLockedCases
                                        else
                                            missedCases
                                            |> List.map (fun fieldName -> MissedCaseInRecord (fullName, unionName, fieldName))
                                            |> Error
                                    )))

                        match res with
                        | Ok (locks,nextNum) -> Ok (OneOf (field.Name, unionName, locks)), nextNum
                        | Error err -> Error err, nextNum

                    | _ -> // regular field
                        match fieldsMap.TryFind field.Name with
                        | None -> // new field
                            Ok (Field {Name = field.Name; Type = field.Type; Num = nextNum}), (nextNum + 1)
                        | Some item ->
                            match item with
                            | Field lock when allowedEvolution lock.Type field.Type ->
                                Ok (Field {Name = field.Name; Type = field.Type; Num = lock.Num}), nextNum
                            | Field lock ->
                                Error [UnacceptableEvolutionOfFieldType(fullName, field.Name, lock.Type, field.Type)], nextNum
                            | OneOf (_, unionName, _) ->
                                Error [UnacceptableEvolutionOfFieldType(fullName, field.Name, Complex unionName, field.Type)], nextNum)
                    (maxNum + 1)
                |> fst
                |> traverse id
                |> Result.bind(fun newLockItems ->

                    messageLockItemsToKeyValuePairs newLockItems
                    |> tryMap
                    |> Result.mapError (List.map (fun fieldName -> DuplicateFieldInRecord (fullName,fieldName)))
                    |> Result.bind (fun newLockItemsMap ->

                        let missedFields =
                            messageLockItems
                            |> List.choose (fun x ->
                                let fieldName = lockItemName x
                                match newLockItemsMap.TryFind fieldName with
                                | Some _ -> None
                                | None -> Some fieldName)

                        if missedFields.IsEmpty then
                            MessageLock {Name = fullName; LockItems = newLockItems} |> Ok
                        else
                            missedFields
                            |> List.map (fun fieldName -> MissedFieldInRecord (fullName, fieldName))
                            |> Error))))