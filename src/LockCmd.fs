[<RequireQualifiedAccess>]
module rec Protogen.LockCmd

open System
open System.IO
open System.Text
open Types
open Codegen

let Handler modules locks = function
    | lockFileName::args ->
        Types.lock modules locks
        |> Result.mapError (sprintf "%A")
        |> Result.bind(fun newlocks ->
            if newlocks <> locks then
                Console.WriteLine($"Updating {lockFileName}")
                File.WriteAllText(lockFileName, (gen newlocks))
            else
                Console.WriteLine($"Lock is not changed for {lockFileName}")
            Ok ())
    | [] -> Error "first argument should contain name of the lockfile"


let Instance = {
    Name = "lock"
    Description = "lock given pgen types, an error is raised if evolution is not possible"
    Run = Handler
}


let rec typeToString (type':Type) =
    match type' with
    | Bool -> "bool"
    | String -> "string"
    | Int -> "int"
    | Long -> "long"
    | Float -> "float"
    | Double -> "double"
    | Decimal scale -> $"decimal{scale}"
    | Bytes -> "bytes"
    | Timespamp -> "timestamp"
    | Duration -> "duration"
    | Guid -> "guid"
    | Optional v -> typeToString v + " option"
    | Array v -> typeToString v + " array"
    | Map v -> typeToString v + " map"
    | Complex ns -> cn ns

let gen (locks:LockItem list) =
    let txt = StringBuilder()

    let rec f = function
        | EnumLock lock ->
            line txt $"enum {cn lock.Name}"
            for value' in lock.Values do
                line txt $"    value {value'.Name} = {value'.Num}"
        | MessageLock lock ->
            line txt $"message {cn lock.Name}"
            for item in lock.LockItems do
                match item with
                | Field lock ->
                    line txt $"    field {lock.Name} {typeToString lock.Type} = {lock.Num}"
                | OneOf (name, unionName, locks) ->
                    line txt $"    oneof {name} {cn unionName}"
                    for case in locks do
                    line txt $"        case {case.CaseName} = {case.Num}"

    locks |> List.map f |> ignore
    txt.ToString()