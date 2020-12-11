[<RequireQualifiedAccess>]
module rec Protogen.ProtoCmd

open System
open System.Text
open System.IO
open Types
open Codegen

let Handler modules locks = function
    | "-o"::outputFileName::args
    | "--output"::outputFileName::args ->
        Types.lock modules locks
        |> Result.mapError (sprintf "When try to check current lock: %A")
        |> Result.bind(fun newlocks ->
            if newlocks <> locks then
                Error "Lock file is not corresponded to types definition. Run protogen lock first."
            else
                let outputTxt = gen modules locks
                let outputFileName =
                    if Path.GetExtension outputFileName <> ".proto" then outputFileName + ".proto" else outputFileName
                Console.WriteLine($"Writing .proto definition to {outputFileName}")
                File.WriteAllText(outputFileName, outputTxt)
                Ok ()
            )
    | x -> Error $"expected arguments [-o|--output] outputfile, but {x}"


let Instance = {
    Name = "proto"
    Description = "generate protobuf description: proto [-o|--output] outputfile"
    Run = Handler
}


let gen (modules:Module list) (locks:LockItem list) =

    let enumLocksCache =
        locks |> List.choose(function EnumLock item -> Some(item.Name, item) | _ -> None) |> Map.ofList

    let txt = StringBuilder()

    let f ns = function
    | Enum info ->
        let fullName = Types.mergeName ns info.Name
        line txt $"enum {info.Name} {{"
        line txt "    Unknown = 0;"
        for symbol in enumLocksCache.[fullName].Values do
            line txt $"    {symbol.Name} = {symbol.Num};"
        line txt $"}}"
    | Record info -> ()
    | Union info -> ()

    line txt "syntax = \"proto3\";"
    modules
    |> List.iter(fun module' ->
        line txt $"package {cn module'.Name};"
        module'.Items |> List.iter (f module'.Name)
    )

    txt.ToString()