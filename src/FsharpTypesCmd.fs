[<RequireQualifiedAccess>]
module rec Protokeep.FsharpTypesCmd

open System
open System.Text
open System.IO
open Types
open Codegen

let Handler module' locks typesCache =
    function
    | "-o" :: outputFileName :: args
    | "--output" :: outputFileName :: args ->
        Program.checkLock module' locks typesCache
        |> Result.bind (fun _ ->
            let fileContent = gen module' locks typesCache

            let fileName =
                if Path.GetExtension(outputFileName) <> ".fs" then
                    outputFileName + ".g.fs"
                else
                    outputFileName

            Console.WriteLine($"Writing fsharp types to {fileName}")
            File.WriteAllText(fileName, fileContent)

            let defaultCommonsFileName =
                Path.Combine(Path.GetDirectoryName(fileName), "Protokeep.fs")

            Program.checkArgUpdateCommons defaultCommonsFileName args
            |> Option.iter (fun coreFileName ->
                let coreFileText =
                    if (File.Exists coreFileName) then
                        File.ReadAllText(coreFileName)
                    else
                        ""

                let updatedCoreFileText = CoreFsharp.update coreFileText "FsharpTypes" commonsBody
                Console.WriteLine($"Writing common fsharp types and helpers to {coreFileName}")
                File.WriteAllText(coreFileName, updatedCoreFileText))

            Ok())
    | x -> Error $"expected arguments [-o|--output] outputFile, but {x}"

let Instance =
    { Name = "fsharp-types"
      Description =
        "generate fsharp types: fsharp-types [-o|--output] outputFile [--update-commons | --update-commons-in commonsFile]"
      Run = Handler }

let gen (module': Module) (locks: LocksCollection) (typesCache: Types.TypesCache) =
    let ns = module'.Name

    let txt = StringBuilder()

    let rec genItem ns =
        function
        | Enum info ->
            line txt $"type {firstName info.Name} ="
            line txt "    | Unknown = 0"

            for symbol in locks.Enum(info.Name).Values do
                line txt $"    | {symbol.Name} = {symbol.Num}"
        | Record info ->
            line txt $"type {firstName info.Name} = {{"

            for field in info.Fields do
                line txt $"    {field.Name} : {typeToString ns field.Type}"

            line txt $"}}"
            let keys = info.Fields |> List.filter (fun x -> x.IsKey)

            let indexes =
                info.Fields
                |> List.collect (fun x -> x.Indexes)
                |> List.map (fun i -> i.Name)
                |> List.distinct

            if not keys.IsEmpty || not indexes.IsEmpty then
                line txt $"with"

                if not keys.IsEmpty then
                    recordKeyMembers ns typesCache txt info.Name keys

                for indexName in indexes do
                    recordIndexMembers locks txt indexName info

        | Union info ->
            line txt $"type {firstName info.Name} ="
            line txt $"    | Unknown"

            for case in info.Cases do
                let fieldsStr =
                    case.Fields
                    |> List.map (fun field -> $"{field.Name}:{typeToString ns field.Type}")
                    |> String.concat "*"
                    |> (fun str -> if str <> "" then " of " + str else str)

                line txt $"    | {firstName case.Name}{fieldsStr}"

            line txt $"with"
            unionKeyMembers ns locks typesCache txt info

            let indexes =
                info.Cases
                |> List.collect (fun i ->
                    i.Fields
                    |> List.collect (Types.referencedIndexes typesCache)
                    |> List.map (fun ii -> ii.Name))
                |> List.distinct

            for indexName in indexes do
                unionIndexMembers typesCache txt indexName info

    line txt $"namespace {dottedName module'.Name}"
    line txt "open Protokeep.FsharpTypes"

    for item in module'.Items do
        genItem module'.Name item

    txt.ToString()

let rec typeToString (ns: ComplexName) (type': Type) =
    match type' with
    | Bool -> "bool"
    | String -> "string"
    | Int -> "int"
    | Long -> "int64"
    | Float -> "float32"
    | Double -> "float"
    | Decimal _ -> "decimal"
    | Bytes -> "byte array"
    | Timestamp -> "System.DateTime"
    | Duration -> "System.TimeSpan"
    | Guid -> "System.Guid"
    | Optional v -> typeToString ns v + " option"
    | Array v -> typeToString ns v + " array"
    | List v -> typeToString ns v + " list"
    | Map v -> $"Map<string,{typeToString ns v}>"
    | Complex typeName ->
        if lastNames typeName = ns then
            firstName typeName
        else
            dottedName typeName

let keyParams ns (typesCache: TypesCache) (keyFields: FieldInfo list) =
    keyFields
    |> List.map (fun info ->
        let pName = info.Name |> firstCharToLower

        match info.Type with
        | Types.IsRecord typesCache _
        | Types.IsUnion typesCache _ -> $"{pName}Key: Key"
        | _ -> $"{pName}': {typeToString ns info.Type}")
    |> String.concat ", "

let caseParams (fields: FieldInfo list) =
    if fields.IsEmpty then
        ""
    else
        fields
        |> List.map (fun info -> $"{info.Name}'")
        |> String.concat ", "
        |> (fun txt -> " (" + txt + ")")

let keyExpression (typesCache: TypesCache) (keyFields: FieldInfo list) =
    keyFields
    |> List.map (fun info ->
        let vName = $"{firstCharToLower info.Name}'"

        match info.Type with
        | String -> $"Key.Value ({vName})"
        | Int
        | Long
        | Decimal _ -> $"Key.Value ({vName}.ToString())"
        | Guid -> $"Key.Value ({vName}.ToString())"
        | Types.IsRecord typesCache _
        | Types.IsUnion typesCache _ -> $"Key.Inner {firstCharToLower info.Name}Key"
        | Types.IsEnum typesCache _ -> $"Key.Value ((int {vName}).ToString())"
        | wrong -> failwithf "type not supported as key %A" wrong)
    |> String.concat "; "

let makeKeyArgs (typesCache: TypesCache) (keyFields: FieldInfo list) prefix suffix =
    keyFields
    |> List.map (fun info ->
        match info.Type with
        | Types.IsRecord typesCache _
        | Types.IsUnion typesCache _ -> $"{prefix}{info.Name}{suffix}.Key"
        | _ -> $"{prefix}{info.Name}{suffix}")
    |> String.concat ", "

let recordKeyMembers ns (typesCache: TypesCache) txt typeName keyFields =
    line txt $"    static member MakeKey ({keyParams ns typesCache keyFields}) ="

    match keyFields with
    | [] -> failwith "empty key fields is not possible"
    | [ _ ] -> line txt $"        {keyExpression typesCache keyFields}"
    | _ -> line txt $"        Key.Items [{keyExpression typesCache keyFields}]"

    let keyArgs = makeKeyArgs typesCache keyFields "x." ""
    line txt "    interface IEntity with"
    line txt $"        member x.Key = {firstName typeName}.MakeKey ({keyArgs})"

let unionKeyMembers ns (locks: LocksCollection) (typesCache: TypesCache) txt (info: UnionInfo) =
    line txt "    static member MakeUnknownKey () = Key.Value \"0\""

    for recordInfo, caseLock in locks.Union(info.Name).Cases |> List.zip info.Cases do
        let keyFields = recordInfo.Fields |> List.filter (fun x -> x.IsKey)

        let keyExpression =
            if keyFields.IsEmpty then
                $"Key.Value \"{caseLock.Num}\""
            else
                $"Key.Items [Key.Value \"{caseLock.Num}\"; {keyExpression typesCache keyFields}]"

        line txt $"    static member Make{caseLock.Name}Key ({keyParams ns typesCache keyFields}) = {keyExpression}"

    line txt "    interface IEntity with"
    line txt "        member x.Key ="
    line txt "            match x with"
    line txt $"            | {firstName info.Name}.Unknown -> {firstName info.Name}.MakeUnknownKey ()"

    for recordInfo, caseLock in locks.Union(info.Name).Cases |> List.zip info.Cases do
        let keyFields = recordInfo.Fields |> List.filter (fun x -> x.IsKey)
        let keyArgs = makeKeyArgs typesCache keyFields "" "'"

        line
            txt
            $"            | {firstName info.Name}.{caseLock.Name}{caseParams recordInfo.Fields} -> {firstName info.Name}.Make{caseLock.Name}Key ({keyArgs})"

let recordIndexMembers (locks: LocksCollection) txt indexName recordInfo =
    let indexedFields =
        recordInfo.Fields
        |> List.zip (locks.Record(recordInfo.Name).Fields)
        |> List.choose (fun (fieldLock, fieldInfo) ->
            fieldInfo.Indexes
            |> List.tryFind (fun ii -> ii.Name = indexName)
            |> Option.map (fun ii -> fieldInfo, ii, fieldLock))

    line txt $"    member x.{firstCharToUpper indexName}Values () ="

    let values' =
        indexedFields
        |> List.map (fun (field, idx, _) ->
            match idx.Value with
            | Self -> $"x.{field.Name}"
            | IndexValue.Field name ->
                match field.Type with
                | Array _
                | List _ -> $"yield! x.{field.Name} |> Seq.map (fun v -> v.{name})"
                | _ -> $"x.{field.Name}.{name}")
        |> String.concat "; "

    line txt $"        [| {values'} |]"

    line txt $"    member x.{firstCharToUpper indexName}Indexes () ="

    let indexes' =
        indexedFields
        |> List.map (fun (field, idx, fieldLock) ->
            match idx.Key with
            | IndexKey.Num -> $"Key.Value \"{fieldLock.Num}\""
            | IndexKey.FieldKey name ->
                match field.Type with
                | Array _
                | List _ -> $"yield! x.{field.Name} |> Seq.map (fun v -> v.{name}.Key)"
                | _ -> $"x.{field.Name}.{name}")
        |> String.concat "; "

    line txt $"        [| {indexes'} |]"

    for fieldInfo, idx, fieldLock in indexedFields do
        match idx.Key with
        | IndexKey.FieldKey name ->
            match fieldInfo.Type with
            | Array _
            | List _ ->
                line txt $"    member x.TryFind{firstCharToUpper indexName}In{fieldInfo.Name} (key:Key) ="
                line txt $"        x.{fieldInfo.Name} |> Seq.tryFind (fun i -> i.{name}.Key = key)"
            | _ -> ()
        | _ -> ()

    line txt $"    member x.{firstCharToUpper indexName} = function"

    for fieldInfo, idx, fieldLock in indexedFields do
        match idx.Key, idx.Value with
        | (Num, IndexValue.Self) -> line txt $"        | Key.Value \"{fieldLock.Num}\" -> Some x.{fieldInfo.Name}"
        | (IndexKey.FieldKey _), (IndexValue.Field name) ->
            line txt $"        | TryFind x.TryFind{firstCharToUpper indexName}In{fieldInfo.Name} v -> Some v.{name}"
        | wrong -> failwithf "Not supported indexer %A" wrong

    line txt $"        | _ -> None"

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
                    $"fun v -> items.TryFind v.{keyName}.Key |> Option.map (fun i -> {{v with {valueName} = i}}) |> Option.defaultValue v"

                line txt $"            {fieldInfo.Name} = x.{fieldInfo.Name} |> Array.map ({mapTxt})"
            | List _ ->
                let mapTxt =
                    $"fun v -> items.TryFind v.{keyName}.Key |> Option.map (fun i -> {{v with {valueName} = i}}) |> Option.defaultValue v"

                line txt $"            {fieldInfo.Name} = x.{fieldInfo.Name} |> List.map ({mapTxt})"
            | wrong -> failwithf "Not supportes type for indexier %A" wrong
        | wrong -> failwithf "Not supported indexer %A" wrong

    line txt $"        }}"

let unionIndexMembers (typesCache: TypesCache) txt indexName unionInfo =
    let hasIndex field =
        Types.referencedIndexes typesCache field
        |> List.tryFind (fun i -> i.Name = indexName)
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

let commonsBody =
    """
    type Key =
        | Value of string
        | Items of Key list
        | Inner of Key

        override x.ToString() =
            match x with
            | Value v -> v
            | Items keys -> keys |> List.map (fun key -> key.ToString()) |> String.concat ","
            | Inner key -> $"({key})"

    let (|TryFind|_|) f key = f key

    type IEntity =
        abstract member Key: Key

"""
