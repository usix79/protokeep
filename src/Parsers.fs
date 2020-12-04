module Protogen.Parsers

open FParsec
open Types

module private Impl =

    let ws<'u> : Parser<_,'u> = skipMany (skipAnyOf " \t")
    let ws1<'u> : Parser<_,'u> = skipMany1 (skipAnyOf " \t")
    let ts<'u> : Parser<_,'u> = ws >>. (skipNewline <|> eof)  // trailing spaces
    let keyword name = pstring name >>. ws1

    let identifier =
        let isIdentifierFirstChar c = isLetter c || c = '_'
        let isIdentifierChar c = isLetter c || isDigit c || c = '_'

        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"

    let complexName : Parser<ComplexName, _> =
        sepBy1 identifier (pchar '.') |>> (List.rev >> ComplexName)

    let enum' =
        keyword "enum" >>.
            pipe2
                (identifier .>> ws .>> skipChar '=' .>> spaces .>> opt (pchar '|'))
                (sepBy1 (between spaces spaces identifier) (pchar '|') )
                (fun name symbols -> Enum {Name = name; Symbols = symbols})

    let type' =
        choice[
            skipString "bool" |>> (fun () -> Bool)
            skipString "string" |>> (fun () -> String)
            skipString "int" |>> (fun () -> Int)
            skipString "long" |>> (fun () -> Long)
            skipString "float" |>> (fun () -> Float)
            skipString "double" |>> (fun () -> Double)
            skipString "bytes" |>> (fun () -> Bytes)
            skipString "timestamp" |>> (fun () -> Timespamp)
            skipString "duration" |>> (fun () -> Duration)
            skipString "guid" |>> (fun () -> Guid)
            skipString "decimal" >>. ws >>. pchar '(' >>. ws >>. pint32 .>> ws .>> pchar ')' |>> Decimal
            complexName |>> Complex]

    let fullType' =
        type' >>= (fun t ->
            ws >>.
            opt (choice[
                    skipString "option" |>> (fun () -> Optional t)
                    skipString "array" |>> (fun () -> Array t)
                    skipString "map" |>> (fun () -> Map t)])
            |>> (Option.defaultValue t))

    let field' =
        pipe2
            ((between spaces ws identifier) .>> pchar ':')
            (between ws ws fullType')
            (fun name type' -> {Name = name; Type = type'} : FieldInfo)

    let record' =
            keyword "record" >>.
                pipe2
                    (identifier .>> ws .>> skipChar '=' .>> spaces)
                    (between (pchar '{') (pchar '}' .>> spaces)
                        (sepEndBy field' (pchar ';' <|> newline)))
                    (fun name fields -> Record {Name = name; Fields = fields})

    let unionCaseField' =
        pipe2
            (opt ((between ws ws identifier) .>>? pchar ':'))
            (between ws ws fullType')
            (fun name type' -> {|Name = name; Type = type'|})

    let unionCase' : Parser<UnionCaseInfo,_> =
        pipe2
            identifier
            (opt (ws1 .>>? (keyword "of") >>. sepBy unionCaseField' (pchar '*')))
            (fun name fields -> {
                    Name = name
                    Fields =
                        fields
                        |> Option.defaultValue []
                        |> List.mapi (fun idx r -> {
                                    Name = r.Name |> Option.defaultWith (fun () -> $"Item{idx+1}")
                                    Type = r.Type })
                        })

    let union' =
        keyword "union" >>.
            pipe2
                (identifier .>> ws .>> skipChar '=' .>> spaces .>> opt (pchar '|'))
                (sepBy1 (between spaces spaces unionCase') (pchar '|') )
                (fun name cases -> Union {Name = name; Cases = cases})

    let module' =
        keyword "module" >>.
            pipe2
                (complexName .>> ts)
                (many (choice [enum'; record'; union']))
                (fun name items -> {Name = name; Items = items})

    let pgenFile =
        spaces >>. many (module' .>> spaces) .>> eof

let parsePgen input =
    match run Impl.pgenFile input with
    | Success (model,_,_) -> Result.Ok model
    | Failure (err,_,_) -> Result.Error err