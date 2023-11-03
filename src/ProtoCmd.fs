[<RequireQualifiedAccess>]
module rec Protokeep.ProtoCmd

open System
open System.Text
open System.IO
open Types
open Codegen

let Handler module' locks typesCache =
    function
    | "-o" :: outputFileName :: args
    | "--output" :: outputFileName :: args ->
        Infra.checkLock module' locks typesCache
        |> Result.bind (fun _ ->
            let fileContent = gen module' locks typesCache

            let fileName =
                if Path.GetExtension(outputFileName) <> ".proto" then
                    outputFileName + ".proto"
                else
                    outputFileName

            Console.WriteLine($"Writing .proto definition to {fileName}")
            File.WriteAllText(fileName, fileContent)
            Ok())
    | x -> Error $"expected arguments [-o|--output] outputFile, but {x}"

let Instance =
    { Name = "proto"
      Description = "generate protobuf description: proto [-o|--output] outputFile"
      Run = Handler }

let keyValuePairName (ns: ComplexName) (k: Type) (v: Type) =
    let rec solid =
        function
        | Complex name -> solidDiff ns name
        | Array v
        | List v
        | Set v -> $"Repeated{solid v}"
        | t -> $"{t}"

    $"{solid k}{solid v}Pair"

let resolveKeyValuePairs (module': Module) =
    let resolveRecord (info: RecordInfo) =
        info.Fields
        |> List.choose
           ^ fun fieldInfo ->
               match fieldInfo.Type with
               | Map(k, v) -> (k, v) |> Some
               | _ -> None

    let rec resolve =
        function
        | Record info -> resolveRecord info
        | Union info -> info.Cases |> List.collect resolveRecord
        | _ -> []

    module'.Items |> List.collect resolve |> List.distinct

let gen (module': Module) (locks: LocksCollection) (typesCache: TypesCache) =
    let txt = StringBuilder()
    let ns = module'.Name

    let rec genItem =
        function
        | Enum info ->
            let name = firstName info.Name
            line txt $"enum {name} {{"
            line txt $"    {name}Unknown = 0;"

            for symbol in locks.Enum(info.Name).Values do
                line txt $"    {name}{symbol.Name} = {symbol.Num};"

            line txt $"}}"
            line txt $""
        | Record info -> genRecord (firstName info.Name) info
        | Union info ->
            line txt $"message {firstName info.Name} {{"
            line txt $"    oneof Union {{"

            for caseInfo, caseLock in locks.Union(info.Name).Cases |> List.zip info.Cases do
                let fieldTypeName =
                    match caseInfo with
                    | Types.EmptyRecord -> "bool" // empty case would be bool
                    | Types.SingleFieldRecord fieldInfo ->
                        match caseNeedsRecord caseInfo with
                        | true -> $"{dottedName info.Name}__{firstName caseInfo.Name}"
                        | false -> $"{typeToString ns fieldInfo.Type}"
                    | Types.MultiFieldsRecord -> $"{dottedName info.Name}__{firstName caseInfo.Name}"

                line txt $"        {fieldTypeName} {caseLock.Name} = {caseLock.Num};"

            line txt $"    }}"
            line txt $"}}"
            line txt $""

            for case in info.Cases |> List.filter caseNeedsRecord do
                let recordName = (firstName info.Name) + "__" + (firstName case.Name)
                genRecord recordName case

    and genRecord recordName info =
        line txt $"message {recordName} {{"

        for item in locks.Record(info.Name).Fields do
            match item.Type with
            | Optional v ->
                line
                    txt
                    $"    oneof {firstCharToUpper item.Name} {{{typeToString ns v} {firstCharToUpper item.Name}Value = {item.Num};}}"
            | _ -> line txt $"    {typeToString ns item.Type} {firstCharToUpper item.Name} = {item.Num};"

        line txt $"}}"
        line txt $""

    line txt """syntax = "proto3";"""
    line txt $"package {dottedName module'.Name};"
    line txt $"option csharp_namespace = \"ProtoClasses.{dottedName module'.Name}\";"

    for reference in references locks module' do
        if reference <> module'.Name then
            line txt $"import \"{dottedName reference}.proto\";"

    module'.Items |> List.iter genItem

    for k, v in resolveKeyValuePairs module' do
        let name = keyValuePairName ns k v
        line txt $"message {name} {{"
        line txt $"    {typeToString ns k} key = 1;"
        line txt $"    {typeToString ns v} value = 2;"
        line txt $"}}"
        line txt $""

    txt.ToString()

let caseNeedsRecord =
    function
    | Types.EmptyRecord -> false
    | Types.SingleFieldRecord fi ->
        match fi.Type with
        | Optional _
        | Array _
        | List _
        | Set _
        | Map _ -> true
        | _ -> false
    | Types.MultiFieldsRecord -> true


let rec typeToString (ns: ComplexName) (type': Type) =
    match type' with
    | Bool -> "bool"
    | String -> "string"
    | Int8 -> "int32"
    | Int16 -> "int32"
    | Int32 -> "int32"
    | Int64 -> "int64"
    | Float32 -> "float"
    | Float64 -> "double"
    | Money _ -> "int32"
    | Binary -> "bytes"
    | Timestamp -> "google.protobuf.Timestamp"
    | Duration -> "google.protobuf.Duration"
    | Guid -> "bytes"
    | Optional v -> typeToString ns v
    | Array v
    | List v
    | Set v -> "repeated " + (typeToString ns v)
    | Map(k, v) -> $"repeated {keyValuePairName ns k v}"
    | Complex ns -> dottedName ns

let references (locks: LocksCollection) (module': Module) =
    let set = Collections.Generic.HashSet<ComplexName>()

    let rec typeReference =
        function
        | Timestamp -> [ ComplexName [ "google/protobuf/timestamp" ] ]
        | Duration -> [ ComplexName [ "google/protobuf/duration" ] ]
        | Complex ns -> [ Types.extractNamespace ns ]
        | Optional v
        | Array v
        | List v
        | Set v -> typeReference v
        | Map(k, v) -> typeReference k @ typeReference v
        | _ -> []

    let rec f =
        function
        | Enum _ -> ()
        | Record info -> fRecord info
        | Union info -> info.Cases |> List.iter fRecord

    and fRecord info =
        info.Fields
        |> List.map (fun fieldInfo -> typeReference fieldInfo.Type)
        |> List.concat
        |> List.iter (fun r -> set.Add(r) |> ignore)

    module'.Items |> List.iter f

    seq {
        for item in set do
            item
    }
