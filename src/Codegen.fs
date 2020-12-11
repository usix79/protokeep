module Protogen.Codegen

open System.Text
open Types

let line (txt:StringBuilder) l = txt.AppendLine(l) |> ignore

let cn (ComplexName ns) = ns |> List.rev |> String.concat "."

let cnHead (ComplexName ns) = ns.Head

let cnTail (ComplexName ns) = ns.Tail |> List.rev |> String.concat "."
