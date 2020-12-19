module Protogen.Codegen

open System
open System.Text
open Types




let line (txt:StringBuilder) l = txt.AppendLine(l) |> ignore

let solidName (ComplexName ns) = ns |> List.rev |> String.concat ""

let dottedName (ComplexName ns) = ns |> List.rev |> String.concat "."

let firstName (ComplexName ns) = ns.Head

let lastNames (ComplexName ns) = ComplexName ns.Tail

let firstCharToUpper (name:string) =
    if name.Length > 0 && Char.IsLower(name.[0]) then
        Char.ToUpper(name.[0]).ToString() + name.Substring(1);
    else name

let firstCharToLower (name:string) =
    if name.Length > 0 && Char.IsUpper(name.[0]) then
        Char.ToLower(name.[0]).ToString() + name.Substring(1);
    else name

module Program =
    let checkLock module' locks typesCache =
        Types.lock module' locks typesCache
        |> Result.mapError (sprintf "When try to check current lock: %A")
        |> Result.bind(fun newlocks ->
            if locks.HasChanges newlocks then
                Error "Lock file is not corresponded to types definition. Run protogen lock first."
            else
                Ok () )

    let rec checkArgCore defaultName = function
        | [] -> None
        | "--update-commons" :: _ -> Some defaultName
        | "--update-commons-in" :: fileName :: _ -> Some fileName
        | _ :: tail -> checkArgCore defaultName tail

module CoreFsharp =
    let construct (modules:(string*string) seq)=
        let txt = StringBuilder()
        line txt "namespace Protogen"
        for moduleName, moduleBody in modules do
            line txt $"module {moduleName} ="
            line txt moduleBody
        txt.ToString()

    let update (coreTxt:string) moduleName moduleBody : string =
        let modules =
            match coreTxt with
            | txt when String.IsNullOrWhiteSpace txt -> [moduleName,moduleBody]
            | txt ->
                match Parsers.parseFsharpCoreDoc txt with
                | Ok modules ->
                    match modules |> List.tryFind (fun (name,_)-> name = moduleName) with
                    | Some _ ->
                        modules
                        |> List.map (fun (name, body) -> if name = moduleName then (name, moduleBody) else (name, body))
                    | None ->
                        modules @ [moduleName, moduleBody]

                | Error err -> failwithf "Parse Core File: %s" err

        construct modules