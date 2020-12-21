module rec Protogen.Types

open Utils

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
    | List of value: Type
    | Map of value: Type
    | Complex of name: ComplexName


type IndexKey =
    | Num
    | FieldKey of string

type IndexValue =
    | Self
    | Field of string

type Index = {Name: string; Key: IndexKey; Value: IndexValue}
type EnumInfo = { Name: ComplexName; Symbols: string list }
type FieldInfo = { Name: string;  Type: Type; IsKey: bool; Indexes: Index list }
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
type RecordFieldLock = { Name: string; Type: Type; Num: int }
type RecordLock = { Name: ComplexName; Fields: RecordFieldLock list}
type UnionCaseLock = { Name: string; Num: int}
type UnionLock = { Name: ComplexName; Cases: UnionCaseLock list}

type LockItem =
    | EnumLock of EnumLock
    | RecordLock of RecordLock
    | UnionLock of UnionLock
    | MessageLock of MessageLock

type TypesCache = Map<ComplexName,ModuleItem>

type LocksCollection (items:LockItem list) =
    let enums = System.Collections.Generic.Dictionary<ComplexName, EnumLock>()
    let records = System.Collections.Generic.Dictionary<ComplexName, RecordLock>()
    let unions = System.Collections.Generic.Dictionary<ComplexName, UnionLock>()
    let messages = System.Collections.Generic.Dictionary<ComplexName, MessageLock>()
    do items |> List.iter (function
        | EnumLock lock -> enums.[lock.Name] <- lock
        | RecordLock lock -> records.[lock.Name] <- lock
        | UnionLock lock -> unions.[lock.Name] <- lock
        | MessageLock lock -> messages.[lock.Name] <- lock )

    member _.TryFind (cn:ComplexName) =
        enums.TryFind cn |> Option.map EnumLock
        |> Option.orElseWith(fun () -> records.TryFind cn |> Option.map RecordLock)
        |> Option.orElseWith(fun () -> unions.TryFind cn |> Option.map UnionLock)
        |> Option.orElseWith(fun () -> messages.TryFind cn |> Option.map MessageLock)

    member _.Enum cn = enums.[cn]
    member _.Record cn = records.[cn]
    member _.Union cn = unions.[cn]
    member _.Message cn = messages.[cn]

    member _.IsEnum cn = enums.ContainsKey cn
    member _.IsMessage cn = messages.ContainsKey cn
    member _.IsRecord cn = records.ContainsKey cn
    member _.IsUnion cn = unions.ContainsKey cn

    member x.HasChanges (items:LockItem list) =
        items
        |> List.tryFind (fun item ->
            match item with
            | EnumLock lock -> enums.TryFind lock.Name |> Option.map ((<>) lock)
            | RecordLock lock -> records.TryFind lock.Name |> Option.map ((<>) lock)
            | UnionLock lock -> unions.TryFind lock.Name |> Option.map ((<>) lock)
            | MessageLock lock -> messages.TryFind lock.Name |> Option.map ((<>) lock)
            |> Option.defaultValue true)
        |> Option.map (fun _ -> true)
        |> Option.defaultValue false

type Command = {
    Name: string
    Description: string
    Run: Module -> LocksCollection -> TypesCache -> string list -> Result<unit,string>
}

module Types =

    open Utils

    type TypeError =
        | UnresolvedType of Type
        | General of string
        | UnacceptableEvolution of recordName:ComplexName*fieldName:string*oldType:Type*newType:Type
        | DuplicateSymbolsInEnum of enumName:ComplexName*symbol:string
        | DuplicateFieldInRecord of recordName:ComplexName*fieldName:string
        | DuplicateSymbolsInLockedEnum of enumName:ComplexName*symbol:string
        | DuplicateFieldInLockedRecord of recordName:ComplexName*fieldName:string
        | DuplicateCaseInLockedUnion of unionName:ComplexName*caseName:string
        | DuplicateFieldInLockedMessage of messageName:ComplexName*fieldName:string
        | MissedSymbolInEnum of enumName:ComplexName*symbol:string
        | MissedFieldInRecord of recordName:ComplexName*fieldName:string
        | MissedCaseInUnion of unionName:ComplexName*fieldName:string
        | UnionNameIsChanged of recordName:ComplexName*fieldName:string*oldName:ComplexName*newName:ComplexName
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
        | RecordLock lock -> lock.Name
        | UnionLock lock -> lock.Name

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

    let referencedIndexes (typesCache:TypesCache) (field:FieldInfo) =
        match field.Type with
        | Complex type' ->
            match typesCache.[type'] with
            | Record info -> info.Fields |> List.collect (fun iii -> iii.Indexes)
            | _ -> []
        | _ -> []


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

        let resolveRecord (info:RecordInfo) =
            info.Fields
            |> traverse (fun fieldInfo ->
                match fieldInfo.Type with
                | Optional (Complex typeName) -> getFullName typeName |> Option.map (Complex >> Optional)
                | Array (Complex typeName) -> getFullName typeName |> Option.map (Complex >> Array)
                | List (Complex typeName) -> getFullName typeName |> Option.map (Complex >> List)
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

    let lock (module': Module) (lockCache: LocksCollection) (typesCache:TypesCache) : Result<LockItem list, TypeError list> =
        module'.Items
        |> traverse (function
            | Enum info ->
                lockEnum lockCache info
                |> Result.map List.singleton
            | Record info ->
                lockRecord lockCache typesCache false info
                |> Result.bind (fun item1 ->
                    lockMessage lockCache typesCache info
                    |> Result.map (fun item2 -> [item1; item2]))
            | Union info ->
                lockUnion lockCache typesCache info
                |> Result.bind (fun unionItem ->
                    info.Cases
                    |> traverse (lockRecord lockCache typesCache true)
                    |> Result.bind(fun recordItems ->
                        info.Cases
                        |> traverse (lockMessage lockCache typesCache)
                        |> Result.map (fun messageItems -> [unionItem; yield! recordItems; yield! messageItems]) ))
        ) |> Result.map List.concat

    let lockEnum (lockCache:LocksCollection) info =
        let valuesLock =
            if lockCache.IsEnum info.Name then lockCache.Enum(info.Name).Values else []

        valuesLock
        |> tryMap getName id
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
            |> Result.map (fun values -> EnumLock {Name = info.Name; Values = values}))

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

    let lockRecord (lockCache:LocksCollection) (typesCache:TypesCache) fieldsAreLocked (recordInfo:RecordInfo) : Result<LockItem, TypeError list> =
        let fieldLocks =
            if lockCache.IsRecord recordInfo.Name then lockCache.Record(recordInfo.Name).Fields else []

        fieldLocks
        |> tryMap getName id
        |> Result.mapError (List.map (fun fieldName -> DuplicateFieldInLockedRecord (recordInfo.Name,fieldName)))
        |> Result.bind (fun fieldsMap ->
            let maxNum = fieldLocks |> List.fold (fun m f -> max m f.Num) 0

            recordInfo.Fields
            |> List.mapFold(fun nextNum field ->
                match fieldsMap.TryFind field.Name with
                | None ->
                    if not fieldsMap.IsEmpty && fieldsAreLocked
                    then Error [AddingFieldIsNotAllowed (recordInfo.Name, field.Name)], nextNum
                    else Ok ({Name = field.Name; Type = field.Type; Num = nextNum}), (nextNum + 1)
                | Some lockedField when allowedEvolution lockedField.Type field.Type ->
                    Ok ({Name = field.Name; Type = field.Type; Num = lockedField.Num}), nextNum
                | Some lockedField ->
                    Error [UnacceptableEvolution(recordInfo.Name, field.Name, lockedField.Type, field.Type)], nextNum )
                (maxNum + 1)
            |> fst
            |> traverse id
            |> Result.bind(fun newFields ->
                let fName (f:RecordFieldLock) = f.Name
                checkMissedItems
                    fName
                    (fun fieldName -> DuplicateFieldInRecord (recordInfo.Name,fieldName))
                    (fun fieldName -> MissedFieldInRecord (recordInfo.Name, fieldName))
                    fieldLocks
                    newFields)
            |> Result.map (fun items -> RecordLock {Name = recordInfo.Name; Fields = items}))

    let lockUnion (lockCache:LocksCollection) (typesCache:TypesCache) (unionInfo:UnionInfo) : Result<LockItem, TypeError list> =
        let caseLocks =
            if lockCache.IsUnion unionInfo.Name then lockCache.Union(unionInfo.Name).Cases else []

        caseLocks
        |> tryMap getName id
        |> Result.mapError (List.map (fun caseName -> DuplicateCaseInLockedUnion (unionInfo.Name, caseName)))
        |> Result.bind (fun casesMap ->
            let maxNum = caseLocks |> List.fold (fun m f -> max m f.Num) 0

            unionInfo.Cases
            |> List.mapFold(fun nextNum case ->
                match casesMap.TryFind (firstName case.Name) with
                | None -> Ok ({Name = firstName case.Name; Num = nextNum}), (nextNum + 1) // new case
                | Some lockedCase -> Ok lockedCase, nextNum )
                (maxNum + 1)
            |> fst
            |> traverse id
            |> Result.bind(
                checkMissedItems
                    getName
                    (fun fieldName -> DuplicateCaseInLockedUnion (unionInfo.Name, fieldName))
                    (fun fieldName -> MissedCaseInUnion (unionInfo.Name, fieldName))
                    caseLocks )
            |> Result.map (fun items -> UnionLock {Name = unionInfo.Name; Cases = items}))

    let lockMessage (lockCache:LocksCollection) (typesCache:TypesCache) info : Result<LockItem, TypeError list> =
        let messageLockItems =
            if lockCache.IsMessage info.Name then lockCache.Message(info.Name).LockItems else []

        messageLockItems
        |> tryMap messageLockItemName id
        |> Result.mapError (List.map (fun fieldName -> DuplicateFieldInLockedMessage (info.Name,fieldName)))
        |> Result.bind (fun fieldsMap ->
            let maxNum = messageLockItems |> List.fold (fun m -> lockItemMaxNum >>  max m) 0

            info.Fields
            |> List.mapFold(fun nextNum field ->
                match field.Type with
                | IsUnion typesCache unionInfo ->
                    match lockUnionCaseMessage info.Name fieldsMap field.Name unionInfo nextNum with
                    | Ok (locks,nextNum) -> Ok (OneOf (field.Name, unionInfo.Name, locks)), nextNum
                    | Error err -> Error err, nextNum
                | _ -> // regular field
                    match fieldsMap.TryFind field.Name with
                    | None -> // new field
                        Ok (Field {Name = field.Name; Type = field.Type; Num = nextNum}), (nextNum + 1)
                    | Some (Field lock) ->
                        Ok (Field {Name = field.Name; Type = field.Type; Num = lock.Num}), nextNum
                    | Some (OneOf (_, lockedUnionName, _)) ->
                        Error [UnacceptableEvolution(info.Name, field.Name, Complex lockedUnionName, field.Type)], nextNum)
                (maxNum + 1)
            |> fst
            |> traverse id
            |> Result.map (fun items -> MessageLock {Name = info.Name; LockItems = items}))

    let lockUnionCaseMessage recordName fieldsMap fieldName info nextNum =
        match fieldsMap.TryFind fieldName with
        | Some (OneOf (_, lockedUnionName, lockedCases)) when lockedUnionName = info.Name -> Ok lockedCases
        | Some (OneOf (_, lockedUnionName, _)) -> Error [UnionNameIsChanged(recordName, fieldName, lockedUnionName, info.Name)]
        | Some (Field lock) -> Error [UnacceptableEvolution(recordName, fieldName, lock.Type, Complex info.Name)]
        | None -> Ok []
        |> Result.bind(fun lockedCases ->
            lockedCases
            |> tryMap (fun c -> c.CaseName) id
            |> Result.mapError (List.map (fun fieldName -> DuplicateFieldInLockedRecord (recordName,fieldName)))
            |> Result.map (fun lockedCasesMap ->
                info.Cases
                |> List.mapFold (fun nextNum case ->
                    match lockedCasesMap.TryFind(firstName case.Name) with
                    | Some lockedCase -> (lockedCase, nextNum)
                    | None -> ({CaseName = firstName case.Name; Num = nextNum}, (nextNum + 1)))
                    nextNum
                ))