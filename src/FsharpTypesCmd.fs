[<RequireQualifiedAccess>]
module rec Protokeep.FsharpTypesCmd

open System.Text
open Types
open Codegen

let Handler module' locks typesCache =
    function
    | "-o" :: outputFileName :: args
    | "--output" :: outputFileName :: args ->
        Infra.checkLock module' locks typesCache
        |> Result.bind
           ^ fun _ ->
               let fileName = gen module' locks typesCache |> Infra.writeFile outputFileName ".fs"
               FsharpHelpers.update "FsharpTypes" |> Infra.updateCommons fileName args
    | x -> Error $"expected arguments [-o|--output] outputFile, but {x}"

let Instance =
    { Name = "fsharp-types"
      Description = "generate fsharp type"
      Run = Handler }

let gen (module': Module) (locks: LocksCollection) (typesCache: Types.TypesCache) =
    let ns = module'.Name

    let txt = StringBuilder()

    let rec genItem ns =
        function
        | Enum info ->
            line txt $"type {firstName info.Name} ="
            line txt $"    | Unknown = 0{literalSuffix info.Type}"

            for symbol in locks.Enum(info.Name).Values do
                line txt $"    | {symbol.Name} = {symbol.Num}{literalSuffix info.Type}"

            line txt $""
        | Record info ->
            if info.IsStruct then
                line txt $"[<Struct>]"

            line txt $"type {firstName info.Name} = {{"

            for field in info.Fields do
                if field.IsVersion then
                    line txt $"    mutable {field.Name} : {typeToString ns field.Type}"
                else
                    line txt $"    {field.Name} : {typeToString ns field.Type}"

            line txt $"}}"

            line txt $"with"
            line txt $"    static member Default: Lazy<{dottedDiff ns info.Name}> ="
            line txt $"        lazy {{"

            for fieldInfo in info.Fields do
                match fieldInfo.Type with
                | Types.IsEnum typesCache enumInfo ->
                    line txt $"            {fieldInfo.Name} = {dottedDiff ns enumInfo.Name}.Unknown"
                | Types.IsUnion typesCache unionInfo ->
                    line txt $"            {fieldInfo.Name} = {dottedDiff ns unionInfo.Name}.Unknown"
                | _ -> line txt $"            {fieldInfo.Name} = {defValue ns false fieldInfo.Type}"

            line txt $"        }}"
            line txt $""

            if info.HasKey then
                recordKeyMembers ns typesCache txt info.Name info.Keys

            for indexName in info.Indexes do
                recordIndexMembers locks txt indexName info

            match info.Fields |> List.tryFind ^ fun x -> x.IsVersion with
            | Some field ->
                line txt $"    interface IVersioned with"
                line txt $"        member x.{field.Name}"
                line txt $"            with get () = x.{field.Name}"
                line txt $"            and set v = x.{field.Name} <- v"
                line txt $""
            | None -> ()


        | Union info ->
            if info.IsStruct then
                line txt $"[<Struct>]"

            line txt $"type {firstName info.Name} ="
            line txt $"    | Unknown"

            for case in info.Cases do
                let fieldsStr =
                    case.Fields
                    |> List.map (fun field -> $"{field.Name}: {typeToString ns field.Type}")
                    |> String.concat " * "
                    |> (fun str -> if str <> "" then " of " + str else str)

                line txt $"    | {firstName case.Name}{fieldsStr}"

            line txt $""

            line
                txt
                $"    static member Default: Lazy<{dottedDiff ns info.Name}> = lazy {dottedDiff ns info.Name}.Unknown"

            line txt $""
            unionKeyMembers ns locks typesCache txt info

            for indexName in info.Indexes typesCache do
                unionIndexMembers typesCache txt indexName info

            line txt $""

    line txt $"namespace {dottedName module'.Name}"
    line txt ""
    line txt "open Protokeep.FsharpTypes"
    line txt ""

    for item in module'.Items do
        genItem module'.Name item

    txt.ToString()

let primitiveTypeToString (type': Type) =
    match type' with
    | Bool -> "bool"
    | String -> "string"
    | Int8 -> "sbyte"
    | Int16 -> "int16"
    | Int32 -> "int"
    | Int64 -> "int64"
    | Float32 -> "float32"
    | Float64 -> "float"
    | Money _ -> "decimal"
    | Binary -> "byte array"
    | Timestamp -> "System.DateTime"
    | Duration -> "System.TimeSpan"
    | Guid -> "System.Guid"
    | _ -> failwithf "type not supported as key %A" type'

let rec typeToString (ns: ComplexName) (type': Type) =
    match type' with
    | Bool -> primitiveTypeToString type'
    | String -> primitiveTypeToString type'
    | Int8 -> primitiveTypeToString type'
    | Int16 -> primitiveTypeToString type'
    | Int32 -> primitiveTypeToString type'
    | Int64 -> primitiveTypeToString type'
    | Float32 -> primitiveTypeToString type'
    | Float64 -> primitiveTypeToString type'
    | Money _ -> primitiveTypeToString type'
    | Binary -> primitiveTypeToString type'
    | Timestamp -> primitiveTypeToString type'
    | Duration -> primitiveTypeToString type'
    | Guid -> primitiveTypeToString type'
    | Optional v -> typeToString ns v + " voption"
    | Array v -> typeToString ns v + " array"
    | List v -> typeToString ns v + " list"
    | Map(k, v) -> $"Map<{typeToString ns k},{typeToString ns v}>"
    | Complex typeName ->
        match lastNames typeName with
        | name when name = ns -> firstName typeName
        | _ -> dottedName typeName

let keyParams ns (typesCache: TypesCache) (keyFields: FieldInfo list) =
    keyFields
    |> List.map
       ^ fun info ->
           let pName = info.Name |> firstCharToLower

           match info.Type with
           | Types.IsRecord typesCache _
           | Types.IsUnion typesCache _ -> $"{pName}Key: Key"
           | _ -> $"{pName}': {typeToString ns info.Type}"
    |> String.concat ", "

let caseParams (fields: FieldInfo list) =
    match fields with
    | [] -> ""
    | fields ->
        fields
        |> List.map ^ fun info -> $"{info.Name}'"
        |> String.concat ", "
        |> fun txt -> $"({txt})"

let keyExpression (typesCache: TypesCache) (keyFields: FieldInfo list) =
    keyFields
    |> List.map
       ^ fun info ->
           let vName = $"{firstCharToLower info.Name}'"

           match info.Type with
           | String -> $"Key.Value ({vName})"
           | Int8
           | Int16
           | Int32
           | Int64
           | Money _ -> $"Key.Value ({vName}.ToString())"
           | Guid -> $"Key.Value ({vName}.ToString())"
           | Types.IsRecord typesCache _
           | Types.IsUnion typesCache _ -> $"Key.Inner {firstCharToLower info.Name}Key"
           | Types.IsEnum typesCache _ -> $"Key.Value ((int {vName}).ToString())"
           | wrong -> failwithf "type not supported as key %A" wrong
    |> String.concat "; "

let makeKeyArgs (typesCache: TypesCache) (keyFields: FieldInfo list) prefix suffix =
    keyFields
    |> List.map
       ^ fun info ->
           match info.Type with
           | Types.IsRecord typesCache _
           | Types.IsUnion typesCache _ -> $"({prefix}{info.Name}{suffix} :> IEntity).Key"
           | _ -> $"{prefix}{info.Name}{suffix}"
    |> String.concat ", "

let recordKeyMembers ns (typesCache: TypesCache) txt typeName keyFields =
    line txt $"    static member MakeKey ({keyParams ns typesCache keyFields}) ="

    match keyFields with
    | [] -> failwith "empty key fields is not possible"
    | [ _ ] -> line txt $"        {keyExpression typesCache keyFields}"
    | _ -> line txt $"        Key.Items [{keyExpression typesCache keyFields}]"

    line txt $""
    line txt $"    interface IEntity with"
    let keyArgs = makeKeyArgs typesCache keyFields "x." ""
    line txt $"        member x.Key = {firstName typeName}.MakeKey ({keyArgs})"
    line txt $""

let unionKeyMembers ns (locks: LocksCollection) (typesCache: TypesCache) txt (info: UnionInfo) =
    line txt "    static member MakeUnknownKey() = Key.Value \"0\""

    for recordInfo, caseLock in locks.Union(info.Name).Cases |> List.zip info.Cases do
        let keyFields = recordInfo.Keys

        let keyExpression =
            match keyFields with
            | [] -> $"Key.Value \"{caseLock.Num}\""
            | keyFields -> $"Key.Items [Key.Value \"{caseLock.Num}\"; {keyExpression typesCache keyFields}]"

        line txt $"    static member Make{caseLock.Name}Key({keyParams ns typesCache keyFields}) = {keyExpression}"

    line txt $""
    line txt $"    interface IEntity with"
    line txt $"        member x.Key ="
    line txt $"            match x with"
    line txt $"            | {firstName info.Name}.Unknown -> {firstName info.Name}.MakeUnknownKey()"

    for recordInfo, caseLock in locks.Union(info.Name).Cases |> List.zip info.Cases do
        let keyFields = recordInfo.Fields |> List.filter (fun x -> x.IsKey)
        let keyArgs = makeKeyArgs typesCache keyFields "" "'"

        let leftSide =
            $"{firstName info.Name}.{caseLock.Name}{caseParams recordInfo.Fields}"

        line txt $"            | {leftSide} -> {firstName info.Name}.Make{caseLock.Name}Key({keyArgs})"

    line txt $""

let recordIndexMembers (locks: LocksCollection) txt indexName recordInfo =
    let indexedFields =
        recordInfo.Fields
        |> List.zip ^ locks.Record(recordInfo.Name).Fields
        |> List.choose
           ^ fun (fieldLock, fieldInfo) ->
               fieldInfo.Indexes
               |> List.tryFind ^ fun ii -> ii.Name = indexName
               |> Option.map ^ fun ii -> fieldInfo, ii, fieldLock

    line txt $"    member x.{firstCharToUpper indexName}Values () ="

    let values' =
        indexedFields
        |> List.map
           ^ fun (field, idx, _) ->
               match idx.Value with
               | Self -> $"x.{field.Name}"
               | IndexValue.Field name ->
                   match field.Type with
                   | Array _
                   | List _ -> $"yield! x.{field.Name} |> Seq.map (fun v -> v.{name})"
                   | _ -> $"x.{field.Name}.{name}"
        |> String.concat "; "

    line txt $"        [| {values'} |]"
    line txt $""

    line txt $"    member x.{firstCharToUpper indexName}Indexes () ="

    let indexes' =
        indexedFields
        |> List.map
           ^ fun (field, idx, fieldLock) ->
               match idx.Key with
               | IndexKey.Num -> $"Key.Value \"{fieldLock.Num}\""
               | IndexKey.FieldKey name ->
                   match field.Type with
                   | Array _
                   | List _ -> $"yield! x.{field.Name} |> Seq.map (fun v -> (v.{name} :> IEntity).Key)"
                   | _ -> $"x.{field.Name}.{name}"
        |> String.concat "; "

    line txt $"        [| {indexes'} |]"
    line txt $""

    for fieldInfo, idx, fieldLock in indexedFields do
        match idx.Key with
        | IndexKey.FieldKey name ->
            match fieldInfo.Type with
            | Array _
            | List _ ->
                line txt $"    member x.TryFind{firstCharToUpper indexName}In{fieldInfo.Name} (key:Key) ="
                line txt $"        x.{fieldInfo.Name} |> Seq.tryFind (fun i -> (i.{name} :> IEntity).Key = key)"
            | _ -> ()
        | _ -> ()

    line txt $""

    line txt $"    member x.{firstCharToUpper indexName} = function"

    for fieldInfo, idx, fieldLock in indexedFields do
        match idx.Key, idx.Value with
        | (Num, IndexValue.Self) -> line txt $"        | Key.Value \"{fieldLock.Num}\" -> Some x.{fieldInfo.Name}"
        | (IndexKey.FieldKey _), (IndexValue.Field name) ->
            line txt $"        | TryFind x.TryFind{firstCharToUpper indexName}In{fieldInfo.Name} v -> Some v.{name}"
        | wrong -> failwithf "Not supported indexer %A" wrong

    line txt $"        | _ -> None"
    line txt $""

    line txt $"    member x.With{firstCharToUpper indexName}s (items:Map<Key,_>) ="
    line txt $"        {{x with"

    for fieldInfo, idx, fieldLock in indexedFields do
        match idx.Key, idx.Value with
        | (Num, IndexValue.Self) ->
            line
                txt
                $"            {fieldInfo.Name} = items.TryFind(Key.Value \"{fieldLock.Num}\") |> Option.defaultValue x.{fieldInfo.Name}"
        | (IndexKey.FieldKey keyName), (IndexValue.Field valueName) ->
            match fieldInfo.Type with
            | Array _ ->
                let mapTxt =
                    $"fun v -> items.TryFind (v.{keyName} :> IEntity).Key |> Option.map (fun i -> {{v with {valueName} = i}}) |> Option.defaultValue v"

                line txt $"            {fieldInfo.Name} = x.{fieldInfo.Name} |> Array.map ({mapTxt})"
            | List _ ->
                let mapTxt =
                    $"fun v -> items.TryFind (v.{keyName} :> IEntity).Key |> Option.map (fun i -> {{v with {valueName} = i}}) |> Option.defaultValue v"

                line txt $"            {fieldInfo.Name} = x.{fieldInfo.Name} |> List.map ({mapTxt})"
            | wrong -> failwithf "Not supportes type for indexier %A" wrong
        | wrong -> failwithf "Not supported indexer %A" wrong

    line txt $"        }}"
    line txt $""

let unionIndexMembers (typesCache: TypesCache) txt indexName unionInfo =
    let hasIndex field =
        Types.referencedIndexes typesCache field
        |> List.tryFind ^ fun i -> i.Name = indexName
        |> (fun i -> i.IsSome)

    line txt $"    member x.{firstCharToUpper indexName}Values () ="
    line txt $"        match x with"
    line txt $"        | {firstName unionInfo.Name}.Unknown -> Array.empty"

    for case in unionInfo.Cases do
        let left =
            $"{firstName unionInfo.Name}.{firstName case.Name}{caseParams case.Fields}"

        let right =
            match case.Fields |> List.tryFind hasIndex with
            | Some field -> $"{field.Name}'.{firstCharToUpper indexName}Values()"
            | None -> "Array.empty"

        line txt $"        | {left} -> {right}"

    line txt $"    member x.{firstCharToUpper indexName}Indexes () ="
    line txt $"        match x with"
    line txt $"        | {firstName unionInfo.Name}.Unknown -> Array.empty"

    for case in unionInfo.Cases do
        let left =
            $"{firstName unionInfo.Name}.{firstName case.Name}{caseParams case.Fields}"

        let right =
            match case.Fields |> List.tryFind hasIndex with
            | Some field -> $"{field.Name}'.{firstCharToUpper indexName}Indexes()"
            | None -> "Array.empty"

        line txt $"        | {left} -> {right}"

    line txt $"    member x.{firstCharToUpper indexName} (key: Key) ="
    line txt $"        match x with"
    line txt $"        | {firstName unionInfo.Name}.Unknown -> None"

    for case in unionInfo.Cases do
        let left =
            $"{firstName unionInfo.Name}.{firstName case.Name}{caseParams case.Fields}"

        let right =
            match case.Fields |> List.tryFind hasIndex with
            | Some field -> $"{field.Name}'.{firstCharToUpper indexName} key"
            | None -> "None"

        line txt $"        | {left} -> {right}"

    line txt $"    member x.With{firstCharToUpper indexName}s (items: Map<Key,_>) ="
    line txt $"        match x with"
    line txt $"        | {firstName unionInfo.Name}.Unknown -> x"

    for case in unionInfo.Cases do
        let left =
            $"{firstName unionInfo.Name}.{firstName case.Name}{caseParams case.Fields}"

        let right =
            match case.Fields |> List.tryFind hasIndex with
            | Some field ->
                let args =
                    case.Fields
                    |> List.map (fun i ->
                        if i = field then
                            $"{i.Name}'.With{firstCharToUpper indexName}s items"
                        else
                            $"{i.Name}'")
                    |> String.concat ", "

                $"{firstName unionInfo.Name}.{firstName case.Name} ({args})"
            | None -> "x"

        line txt $"        | {left} -> {right}"

let literalSuffix =
    function
    | Int8 -> "y"
    | Int16 -> "s"
    | Int32 -> ""
    | Int64 -> "L"
    | Float32 -> "f"
    | Float64 -> ""
    | Money _ -> "m"
    | _ -> ""

let defValue (ns: ComplexName) isMutable t =
    match t with
    | Bool -> "false"
    | String -> "\"\""
    | Int8 -> $"0{literalSuffix t}"
    | Int16 -> $"0{literalSuffix t}"
    | Int32 -> $"0{literalSuffix t}"
    | Int64 -> $"0{literalSuffix t}"
    | Float32 -> $"0.{literalSuffix t}"
    | Float64 -> $"0.{literalSuffix t}"
    | Money _ -> $"0{literalSuffix t}"
    | Binary -> "Array.empty"
    | Timestamp -> "System.DateTime.MinValue"
    | Duration -> "System.TimeSpan.Zero"
    | Guid -> "System.Guid.Empty"
    | Optional _ -> "ValueNone"
    | Array _ -> if isMutable then "ResizeArray()" else "Array.empty"
    | List _ -> if isMutable then "ResizeArray()" else "List.empty"
    | Map _ -> if isMutable then "ResizeArray()" else "Map.empty"
    | Complex typeName when ns = (lastNames typeName) -> $"{typeName |> firstName}.Default.Value"
    | Complex typeName -> $"{typeName |> dottedName}.Default.Value"

let fromMutable =
    function
    | Array _ -> " |> Array.ofSeq"
    | List _ -> " |> List.ofSeq"
    | Map _ -> " |> Map.ofSeq"
    | _ -> ""
