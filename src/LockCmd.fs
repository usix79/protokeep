[<RequireQualifiedAccess>]
module rec Protokeep.LockCmd

open System
open System.IO
open System.Text
open Types
open Codegen

let Handler module' locks typesCache =
    function
    | lockFileName :: args ->
        Types.lock module' locks typesCache
        |> Result.mapError (sprintf "%A")
        |> Result.bind (fun newlocks ->
            if locks.HasChanges newlocks then
                Console.WriteLine($"Updating {lockFileName}")
                File.WriteAllText(lockFileName, (gen newlocks))
            else
                Console.WriteLine($"Lock is not changed for {lockFileName}")

            Ok())
    | [] -> Error "first argument should contain name of the lockfile"

let Instance =
    { Name = "lock"
      Description = "lock given protokeep types, an error is raised if evolution is not possible"
      Run = Handler }

let rec typeToString (type': Type) =
    match type' with
    | Bool -> "bool"
    | String -> "string"
    | Int8 -> "int8"
    | Int16 -> "int16"
    | Int32 -> "int32"
    | Int64 -> "int64"
    | Float32 -> "float32"
    | Float64 -> "float64"
    | Money scale -> $"money({scale})"
    | Binary -> "binary"
    | Timestamp -> "timestamp"
    | Duration -> "duration"
    | Guid -> "guid"
    | Optional v -> $"option<{typeToString v}>"
    | Array v -> $"array<{typeToString v}>"
    | List v -> $"list<{typeToString v}>"
    | Set v -> $"set<{typeToString v}>"
    | Map(k, v) -> $"map<{typeToString k}, {typeToString v}>"
    | Complex ns -> dottedName ns

let gen (locks: LockItem list) =
    let txt = StringBuilder()

    let rec f =
        function
        | EnumLock lock ->
            line txt $"enum {typeToString lock.Type} {dottedName lock.Name}"

            for value' in lock.Values do
                line txt $"    value {value'.Name} = {value'.Num}"
        | RecordLock lock ->
            line txt $"record {dottedName lock.Name}"

            for field in lock.Fields do
                line txt $"    field {field.Name} {typeToString field.Type} = {field.Num}"
        | UnionLock lock ->
            line txt $"union {dottedName lock.Name}"

            for case in lock.Cases do
                line txt $"    case {case.Name} = {case.Num}"

    locks |> List.map f |> ignore
    txt.ToString()
