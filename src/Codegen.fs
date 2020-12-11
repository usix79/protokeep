module Protogen.Codegen

open System
open System.Text
open Types

let line (txt:StringBuilder) l = txt.AppendLine(l) |> ignore

let cn (ComplexName ns) = ns |> List.rev |> String.concat "."

let cnHead (ComplexName ns) = ns.Head

let cnTail (ComplexName ns) = ns.Tail |> List.rev |> String.concat "."

let firstCharToUpper (name:string) =
    if name.Length > 0 && Char.IsLower(name.[0]) then
        Char.ToUpper(name.[0]).ToString() + name.Substring(1);
    else name
