[<RequireQualifiedAccess>]
module rec Protogen.FsharpTypesCmd

open System
open System.Text
open System.IO
open Types
open Codegen

let Handler module' locks = function
    | "-o"::outputFileName::args
    | "--output"::outputFileName::args ->
        Program.checkLock module' locks
        |> Result.bind(fun typesCache ->
            let fileContent = gen module' locks typesCache
            let fileName =
                if Path.GetExtension(outputFileName) <> ".fs" then  outputFileName + ".g.fs" else outputFileName
            Console.WriteLine($"Writing fsharp types to {fileName}")
            File.WriteAllText(fileName, fileContent)
            Ok () )
    | x -> Error $"expected arguments [-o|--output] outputFile, but {x}"

let Instance = {
    Name = "fsharp-types"
    Description = "generate fsharp types: fsharp-types [-o|--output] outputFile"
    Run = Handler
}

let gen (module':Module) (locks:LockItem list) (typesCache:Types.TypesCache) =
    let enumLocksCache =
        locks |> List.choose(function EnumLock item -> Some(item.Name, item) | _ -> None) |> Map.ofList

    let txt = StringBuilder()

    let rec genItem ns = function
    | Enum info ->
        let fullName = Types.mergeName ns info.Name
        line txt $"type {info.Name} ="
        line txt "    | Unknown = 0"
        for symbol in enumLocksCache.[fullName].Values do
            line txt $"    | {symbol.Name} = {symbol.Num}"
    | Record info ->
        line txt $"type {info.Name} = {{"
        for field in info.Fields do
            line txt $"    {field.Name} : {Types.toFullQualifiedType typesCache ns field.Type |> typeToString}"
        line txt $"}}"
    | Union info ->
        line txt $"type {info.Name} ="
        line txt $"    | Unknown"
        for case in info.Cases do
            let fieldsStr =
                case.Fields
                |> List.map (fun field -> $"{field.Name}:{Types.toFullQualifiedType typesCache ns field.Type |> typeToString}")
                |> String.concat "*"
                |> (fun str -> if str <> "" then " of " + str else str)
            line txt $"    | {case.Name}{fieldsStr}"

    line txt $"module rec {dottedName module'.Name}"
    for item in module'.Items do
        genItem module'.Name item

    txt.ToString()

let rec typeToString (type':Type) =
    match type' with
    | Bool -> "bool"
    | String -> "string"
    | Int -> "int"
    | Long -> "int64"
    | Float -> "float32"
    | Double -> "float"
    | Decimal _ -> "decimal"
    | Bytes -> "byte array"
    | Timestamp -> "System.DateTimeOffset"
    | Duration -> "System.TimeSpan"
    | Guid -> "System.Guid"
    | Optional v -> typeToString v + " option"
    | Array v -> typeToString v + " array"
    | Map v -> $"Map<string,{typeToString v}>"
    | Complex ns -> dottedName ns