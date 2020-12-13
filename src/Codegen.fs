module Protogen.Codegen

open System
open System.Text
open Types


let checkLock module' locks =
    Types.lockInternal module' locks
    |> Result.mapError (sprintf "When try to check current lock: %A")
    |> Result.bind(fun (newlocks, typesCache) ->
        if newlocks <> locks then
            Error "Lock file is not corresponded to types definition. Run protogen lock first."
        else
            Ok typesCache )


let line (txt:StringBuilder) l = txt.AppendLine(l) |> ignore

let solidName (ComplexName ns) = ns |> List.rev |> String.concat ""

let dottedName (ComplexName ns) = ns |> List.rev |> String.concat "."

let firstName (ComplexName ns) = ns.Head

let lastNames (ComplexName ns) = ComplexName ns.Tail

let firstCharToUpper (name:string) =
    if name.Length > 0 && Char.IsLower(name.[0]) then
        Char.ToUpper(name.[0]).ToString() + name.Substring(1);
    else name
