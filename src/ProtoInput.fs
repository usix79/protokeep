module Protogen.Input

open FParsec

type ModuleName = string list

type Module = {
    Name: ModuleName
}

module private Parser =

    let identifier =
        let isIdentifierFirstChar c = isLetter c || c = '_'
        let isIdentifierChar c = isLetter c || isDigit c || c = '_'

        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"

    let moduleName : Parser<ModuleName, unit> =
        sepBy1 identifier (pchar '.')

    let module' =
        pstring "module" >>. spaces >>. moduleName
        |>> (fun name -> {Name = name})
        .>> spaces

    let file =
        spaces >>. many module' .>> eof

let parse input =
    match run Parser.file input with
    | Success (model,_,_) -> Result.Ok model
    | Failure (err,_,_) -> Result.Error err