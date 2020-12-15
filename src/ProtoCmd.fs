[<RequireQualifiedAccess>]
module rec Protogen.ProtoCmd

open System
open System.Text
open System.IO
open Types
open Codegen

let Handler module' locks = function
    | "-o"::outputFileName::args
    | "--output"::outputFileName::args ->
        checkLock module' locks
        |> Result.bind(fun _ ->
            let fileContent = gen module' locks
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

let gen (module':Module) (locks:LockItem list) =

    let enumLocksCache =
        locks |> List.choose(function EnumLock item -> Some(item.Name, item) | _ -> None) |> Map.ofList
    let messageLockCache =
        locks |> List.choose(function MessageLock item -> Some(item.Name, item) | _ -> None) |> Map.ofList

    let txt = StringBuilder()

    let rec genItem ns = function
    | Enum info ->
        let fullName = Types.mergeName ns info.Name
        line txt $"enum {info.Name} {{"
        line txt "    Unknown = 0;"
        for symbol in enumLocksCache.[fullName].Values do
            line txt $"    {symbol.Name} = {symbol.Num};"
        line txt $"}}"
    | Record info -> genRecord ns info.Name info
    | Union info ->
        for case in info.Cases do
            if not case.Fields.IsEmpty then
                let recordName = info.Name + "__" + case.Name
                genRecord (Types.mergeName ns info.Name) recordName case
    and genRecord ns recordName info =
        let fullName = Types.mergeName ns info.Name
        line txt $"message {recordName} {{"
        for item in messageLockCache.[fullName].LockItems do
            match item with
            | Field info ->
                match info.Type with
                | Optional v ->
                    line txt $"    oneof {firstCharToUpper info.Name} {{{typeToString v} {firstCharToUpper info.Name}Value = {info.Num};}}"
                | _ ->
                    line txt $"    {typeToString info.Type} {firstCharToUpper info.Name} = {info.Num};"
            | OneOf (name,unionName,fields) ->
                line txt $"    oneof {firstCharToUpper name} {{"
                for fieldLock in fields do
                    let fieldMessage = messageLockCache.[Types.mergeName unionName fieldLock.CaseName]
                    let fieldMessageName =
                        if fieldMessage.LockItems.IsEmpty then "google.protobuf.Empty"
                        else $"{dottedName unionName}__{fieldLock.CaseName}"
                    line txt $"        {fieldMessageName} {firstCharToUpper name}{fieldLock.CaseName} = {fieldLock.Num};"
                line txt $"    }}"
        line txt $"}}"

    line txt """syntax = "proto3";"""
    line txt $"package {dottedName module'.Name};"
    line txt $"option csharp_namespace = \"ProtoClasses.{dottedName module'.Name}\";";
    for reference in references messageLockCache module' do
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
    | Array v -> "repeated " + (typeToString v)
    | Map v -> $"map<string,{typeToString v}>"
    | Complex ns -> dottedName ns

let references (messageLockCache : Map<ComplexName,MessageLock>) (module':Module) =
    let set = Collections.Generic.HashSet<ComplexName>()

    let rec typeReference = function
        | Timestamp -> Some <| ComplexName ["google/protobuf/timestamp.proto"]
        | Duration -> Some <| ComplexName ["google/protobuf/duration.proto"]
        | Complex ns ->  Some <| Types.extractNamespace ns
        | Optional v
        | Array v -> typeReference v
        | _ -> None

    let rec f ns = function
        | Enum _ -> ()
        | Record info -> fRecord ns info
        | Union info ->
            let ns = Types.mergeName ns info.Name
            info.Cases |> List.iter (fRecord ns)
    and fRecord ns info =
        messageLockCache.[Types.mergeName ns info.Name].LockItems
        |> List.collect (function
            | Field x -> [typeReference x.Type]
            | OneOf (_,unionName,cases) ->
                [   Types.extractNamespace unionName |> Some
                    for case in cases do
                        if messageLockCache.[Types.mergeName unionName case.CaseName].LockItems.IsEmpty then
                            ComplexName ["google/protobuf/empty.proto"] |> Some
                ] )
        |> List.choose id
        |> List.iter (fun r -> set.Add(r) |> ignore)

    module'.Items |> List.iter (f module'.Name)

    seq {for item in set do item}