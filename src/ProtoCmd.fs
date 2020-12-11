[<RequireQualifiedAccess>]
module rec Protogen.ProtoCmd

open System
open System.Text
open System.IO
open Types
open Codegen

let wellKnownTypes = ["google.protobuf.duration.proto"; "google.protobuf.timestamp.proto";]


let Handler modules locks = function
    | "-o"::outputDirName::args
    | "--output"::outputDirName::args ->
        Types.lock modules locks
        |> Result.mapError (sprintf "When try to check current lock: %A")
        |> Result.bind(fun newlocks ->
            if newlocks <> locks then
                Error "Lock file is not corresponded to types definition. Run protogen lock first."
            else
                for (fileName,fileContent) in gen modules locks do
                    let fullName = Path.Combine(outputDirName, fileName) + ".proto"
                    Console.WriteLine($"Writing .proto definition to {fullName}")
                    File.WriteAllText(fullName, fileContent)
                // write whell known types
                let assembly = Reflection.Assembly.GetExecutingAssembly();
                for t in wellKnownTypes do
                    let stream = assembly.GetManifestResourceStream("Protogen." + t);
                    let path = Path.Combine(outputDirName, t)
                    use outputFileStream = new FileStream(path, FileMode.Create)
                    stream.CopyTo(outputFileStream);
                Ok ()
            )
    | x -> Error $"expected arguments [-o|--output] outputDirectory, but {x}"


let Instance = {
    Name = "proto"
    Description = "generate protobuf description: proto [-o|--output] outputDirectory"
    Run = Handler
}

let gen (modules:Module list) (locks:LockItem list) =

    let enumLocksCache =
        locks |> List.choose(function EnumLock item -> Some(item.Name, item) | _ -> None) |> Map.ofList
    let messageLockCache =
        locks |> List.choose(function MessageLock item -> Some(item.Name, item) | _ -> None) |> Map.ofList

    let rec f txt ns = function
    | Enum info ->
        let fullName = Types.mergeName ns info.Name
        line txt $"enum {info.Name} {{"
        line txt "    Unknown = 0;"
        for symbol in enumLocksCache.[fullName].Values do
            line txt $"    {symbol.Name} = {symbol.Num};"
        line txt $"}}"
    | Record info -> fRecord txt ns info.Name info
    | Union info ->
        for case in info.Cases do
            let recordName = info.Name + "__" + case.Name
            fRecord txt (Types.mergeName ns info.Name) recordName case
    and fRecord txt ns recordName info =
        let fullName = Types.mergeName ns info.Name
        line txt $"message {recordName} {{"
        printfn "NAME: %A" fullName
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
                    line txt $"        {cn unionName}__{fieldLock.CaseName} {firstCharToUpper name}{fieldLock.CaseName} = {fieldLock.Num};"
                line txt $"    }}"
        line txt $"}}"


    modules
    |> List.map(fun module' ->
        let txt = StringBuilder()
        line txt """syntax = "proto3";"""
        line txt $"package {cn module'.Name};"
        for reference in references messageLockCache module' do
            if reference <> module'.Name then
                line txt $"import \"{cn reference}\";"

        module'.Items |> List.iter (f txt module'.Name)
        (cn module'.Name, txt.ToString()))

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
    | Complex ns -> cn ns

let timestampName = ComplexName ["proto"; "timestamp"; "protobuf"; "google"]
let durationName = ComplexName ["proto"; "duration"; "protobuf"; "google"]

let references (messageLockCache : Map<ComplexName,MessageLock>) (module':Module) =
    let set = Collections.Generic.HashSet<ComplexName>()

    let rec typeReference = function
        | Timestamp -> Some timestampName
        | Duration -> Some durationName
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
        |> List.choose (function
            | Field x -> typeReference x.Type
            | OneOf (_,unionName,_) -> Types.extractNamespace unionName |> Some)
        |> List.iter (fun r -> set.Add(r) |> ignore)

    module'.Items |> List.iter (f module'.Name)

    seq {for item in set do item}