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
type UnionCaseInfo = {Name: string; Fields: FieldInfo list}
type UnionInfo = {Name: string;  Cases: UnionCaseInfo list}

type ModuleItem =
    | Enum of EnumInfo
    | Record of RecordInfo
    | Union of UnionInfo

type Module = {
    Name: ComplexName
    Items: ModuleItem list
}

type EnumValueLock = {Name: string; Num: int}
type EnumLock = { Name: ComplexName; Values: EnumValueLock list}
type MessageFieldLock = { Name: string;  Type: Type; Num: int}
type MessageLock = {Name: ComplexName; Fields: MessageFieldLock list}

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
                        | Enum info -> lockEnum lockCache module'.Name info
                        | Record info -> lockRecord lockCache typesCache module'.Name info
                        | _ -> Ok Empty)

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
            | Union x -> mergeName ns x.Name, item ]

    let valueLocksToKeyValuePairs (items: EnumValueLock list) =
        items |> List.map (fun x -> x.Name, x)

    let fieldLocksToKeyValuePairs (items: MessageFieldLock list) =
        items |> List.map (fun x -> x.Name, x)

    let lockEnum (lockCache:Map<ComplexName,LockItem>) ns info =

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
                let maxNum =
                    if valuesLock.IsEmpty then 0
                    else (valuesLock |> List.maxBy (fun x -> x.Num)).Num

                let newValuesLock =
                    info.Symbols
                    |> List.mapFold(fun nextNum symbol ->
                        match symbolsMap.TryFind symbol with
                        | Some lock -> lock,nextNum
                        | None -> {Name = symbol; Num = nextNum}, (nextNum + 1)) (maxNum + 1)
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

    let tryFindType (typesCache:Map<ComplexName,ModuleItem>) (ComplexName(ns)) (ComplexName (typeName)) =
        let rec f ns =
            typesCache.TryFind(ComplexName (typeName @ ns))
            |> Option.orElseWith(fun () ->
                match ns with
                | [] -> None
                | head::tail -> f tail)
        f ns

    let getUnknownFieldTypes (typesCache:Map<ComplexName,ModuleItem>) ns (info:RecordInfo) =
        [ for field in info.Fields do
            match field.Type with
            | Complex typeName
            | Optional (Complex typeName)
            | Array (Complex typeName)
            | Map (Complex typeName) ->
                match tryFindType typesCache ns typeName with
                | None -> typeName
                | _ -> ()
            | _ -> () ]

    let lockRecord (lockCache:Map<ComplexName,LockItem>) (typesCache:Map<ComplexName,ModuleItem>) ns info : Result<LockItem, Error list> =
        let fullName = mergeName ns info.Name

        match lockCache.TryFind fullName with
        | Some (MessageLock lock) -> Ok lock.Fields
        | Some _ -> [DifferenTypeIsLockedWithThatName fullName] |> Error
        | None -> Ok [] // new record
        |> Result.bind(fun fieldsLock ->
            fieldLocksToKeyValuePairs fieldsLock
            |> tryMap
            |> Result.mapError (List.map (fun fieldName -> DuplicateFieldInLockedMessage (fullName,fieldName)))
            |> Result.bind (fun fieldsMap ->
                let maxNum =
                    if fieldsLock.IsEmpty then 0
                    else (fieldsLock |> List.maxBy (fun x -> x.Num)).Num

                let newFieldsLock =
                    info.Fields
                    |> List.mapFold(fun nextNum field ->
                        match fieldsMap.TryFind (field.Name) with
                        | Some lock ->
                            lock,nextNum // todo: check type mutation
                        | None ->
                            {Name = field.Name; Type = field.Type; Num = nextNum}, (nextNum + 1)) (maxNum + 1)
                    |> fst

                failwith "NYI"))
