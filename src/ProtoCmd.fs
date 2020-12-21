[<RequireQualifiedAccess>]
module rec Protogen.ProtoCmd

open System
open System.Text
open System.IO
open Types
open Codegen

let Handler module' locks typesCache = function
    | "-o"::outputFileName::args
    | "--output"::outputFileName::args ->
        Program.checkLock module' locks typesCache
        |> Result.bind(fun _ ->
            let fileContent = gen module' locks typesCache
            let fileName =
                if Path.GetExtension(outputFileName) <> ".proto" then outputFileName + ".proto" else outputFileName
            Console.WriteLine($"Writing .proto definition to {fileName}")
            File.WriteAllText(fileName, fileContent)
            Ok () )
    | x -> Error $"expected arguments [-o|--output] outputFile, but {x}"

let Instance = {
    Name = "proto"
    Description = "generate protobuf description: proto [-o|--output] outputFile"
    Run = Handler
}

let gen (module':Module) (locks:LocksCollection) (typesCache:TypesCache) =
    let txt = StringBuilder()

    let rec genItem ns = function
    | Enum info ->
        let name = firstName info.Name
        line txt $"enum {name} {{"
        line txt $"    {name}Unknown = 0;"
        for symbol in locks.Enum(info.Name).Values do
            line txt $"    {name}{symbol.Name} = {symbol.Num};"
        line txt $"}}"
    | Record info -> genRecord (firstName info.Name) info
    | Union info ->
        line txt $"message {firstName info.Name} {{"
        line txt $"    oneof Union {{"
        for caseInfo,caseLock in locks.Union(info.Name).Cases |> List.zip info.Cases do
            let fieldTypeName =
                match caseInfo with
                | Types.EmptyRecord -> "bool" // empty case would be bool
                | Types.SingleFieldRecord fieldInfo -> $"{typeToString fieldInfo.Type}"
                | Types.MultiFieldsRecord -> $"{dottedName info.Name}__{firstName caseInfo.Name}"
            line txt $"        {fieldTypeName} {caseLock.Name} = {caseLock.Num};"
        line txt $"    }}"
        line txt $"}}"

        for case in info.Cases do
            let needRecord =
                match locks.Message(case.Name).LockItems with
                | Types.EmptyCase -> false
                | Types.SingleParamCase _ -> false
                | Types.MultiParamCase -> true

            if needRecord then
                let recordName = (firstName info.Name) + "__" + (firstName case.Name)
                genRecord recordName case

    and genRecord recordName info =
        line txt $"message {recordName} {{"
        for item in locks.Record(info.Name).Fields do
            match item.Type with
            | Optional v ->
                line txt $"    oneof {firstCharToUpper item.Name} {{{typeToString v} {firstCharToUpper item.Name}Value = {item.Num};}}"
            | _ ->
                line txt $"    {typeToString item.Type} {firstCharToUpper item.Name} = {item.Num};"
        line txt $"}}"

    line txt """syntax = "proto3";"""
    line txt $"package {dottedName module'.Name};"
    line txt $"option csharp_namespace = \"ProtoClasses.{dottedName module'.Name}\";";
    for reference in references locks module' do
        if reference <> module'.Name then
            line txt $"import \"{dottedName reference}\";"

    module'.Items |> List.iter (genItem module'.Name)
    txt.ToString()

let rec typeToString (type':Type) =
    match type' with
    | Bool -> "bool"
    | String -> "string"
    | Int -> "int32"
    | Long -> "int64"
    | Float -> "float"
    | Double -> "double"
    | Decimal _ -> "int64"
    | Bytes -> "bytes"
    | Timestamp -> "google.protobuf.Timestamp"
    | Duration -> "google.protobuf.Duration"
    | Guid -> "bytes"
    | Optional v -> typeToString v
    | Array v | List v -> "repeated " + (typeToString v)
    | Map v -> $"map<string,{typeToString v}>"
    | Complex ns -> dottedName ns

let references (locks : LocksCollection) (module':Module) =
    let set = Collections.Generic.HashSet<ComplexName>()

    let rec typeReference = function
        | Timestamp -> Some <| ComplexName ["google/protobuf/timestamp.proto"]
        | Duration -> Some <| ComplexName ["google/protobuf/duration.proto"]
        | Complex ns -> Some <| Types.extractNamespace ns
        | Optional v
        | Map v
        | Array v -> typeReference v
        | _ -> None

    let rec f = function
        | Enum _ -> ()
        | Record info -> fRecord info
        | Union info ->
            info.Cases |> List.iter fRecord
    and fRecord info =
        locks.Message(info.Name).LockItems
        |> List.choose (function
            | Field x -> typeReference x.Type
            | OneOf (_,unionName,_) -> Types.extractNamespace unionName |> Some )
        |> List.iter (fun r -> set.Add(r) |> ignore)

    module'.Items |> List.iter f

    seq {for item in set do item}