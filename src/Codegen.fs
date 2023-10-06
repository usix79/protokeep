module Protokeep.Codegen

open System
open System.Text
open Types
open Infra

let line (txt: StringBuilder) (l: string) = txt.AppendLine(l) |> ignore

let linei (txt: StringBuilder) (i: int) (l: string) =
    for _ in 1 .. i * 4 do
        txt.Append(' ') |> ignore

    txt.AppendLine(l) |> ignore

let solidName (ComplexName ns) = ns |> List.rev |> String.concat ""

let dottedName (ComplexName ns) = ns |> List.rev |> String.concat "."

let dottedDiff (ComplexName currentNamespace) (ComplexName typeName) =
    let rec loop =
        function
        | n1 :: ns1, n2 :: ns2 when n1 = n2 -> loop (ns1, ns2)
        | _, ns2 -> ns2

    loop ((currentNamespace |> List.rev), (typeName |> List.rev))
    |> String.concat "."

let firstName (ComplexName ns) = ns.Head

let lastNames (ComplexName ns) = ComplexName ns.Tail

let firstCharToUpper (name: string) =
    if name.Length > 0 && Char.IsLower(name.[0]) then
        Char.ToUpper(name.[0]).ToString() + name.Substring(1)
    else
        name

let firstCharToLower (name: string) =
    if name.Length > 0 && Char.IsUpper(name.[0]) then
        Char.ToLower(name.[0]).ToString() + name.Substring(1)
    else
        name


module FsharpHelpers =
    open FParsec

    type ModuleDefinition = { Name: string; Body: string }

    type FileDefinition =
        { Namespace: string
          Modules: ModuleDefinition list }

    let ws<'u> : Parser<_, 'u> = skipMany (skipAnyOf " \t")
    let wsnl<'u> : Parser<_, 'u> = ws >>. many (skipNewline <|> eof)

    let identifier =
        many1Satisfy (fun c -> isLetter c || isDigit c || c = '.' || c = '_')
        |>> System.String

    let namespaceParser = pstring "namespace" >>. ws >>. identifier

    let moduleBodyParser =
        many1Till (restOfLine true) (lookAhead ^ pstring "module" <|> (eof >>% ""))
        |>> fun lines -> String.concat "\n" lines

    let moduleParser =
        pstring "module" >>. ws >>. identifier .>> ws .>> pstring "=" .>> wsnl
        .>>. moduleBodyParser
        |>> fun (name, body) ->
            { Name = name
              Body = $"module {name} =\n" + body }

    let fileParser =
        namespaceParser .>> wsnl .>>. many1 moduleParser
        |>> fun (ns, modules) -> { Namespace = ns; Modules = modules }


    let construct (ns: string) (modules: string seq) =
        let txt = StringBuilder()
        line txt $"namespace {ns}"
        line txt ""

        for moduleBody in modules do
            line txt ^ moduleBody.TrimEnd()
            line txt ""

        txt.ToString()

    let update moduleName (text: string) : string =
        let embeddedBody = loadEmbeddedFile $"{moduleName}.fs"

        let ns, modules =
            match text with
            | txt when String.IsNullOrWhiteSpace txt -> "Protokeep", [ embeddedBody ]
            | txt ->
                match run fileParser txt with
                | Success(fileDefinition, _, _) ->
                    match
                        fileDefinition.Modules
                        |> List.tryFind ^ fun module' -> module'.Name = moduleName
                    with
                    | Some _ ->
                        fileDefinition.Namespace,
                        fileDefinition.Modules
                        |> List.map
                           ^ fun moduleDefinition ->
                               if moduleDefinition.Name = moduleName then
                                   embeddedBody
                               else
                                   moduleDefinition.Body
                    | None ->
                        fileDefinition.Namespace,
                        fileDefinition.Modules
                        |> List.map ^ fun moduleDefinition -> moduleDefinition.Body
                        |> fun lst -> lst @ [ embeddedBody ]

                | Failure(err, _, _) -> failwithf "Parse common file: %s" err

        construct ns modules
