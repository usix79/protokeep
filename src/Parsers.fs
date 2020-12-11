[<RequireQualifiedAccess>]
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
            skipString "timestamp" |>> (fun () -> Timestamp)
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

    let unionCase' : Parser<RecordInfo,_> =
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
                (many (spaces >>. choice [enum'; record'; union']))
                (fun name items -> {Name = name; Items = items})

    let pgenDocument =
        spaces >>. many (module' .>> spaces) .>> eof

    let enumLock =
        keyword "enum" >>.
            pipe2
                (complexName .>> ts)
                (many1 (
                    (ws >>. pstring "value" >>. ws1 >>.
                        pipe2
                            identifier
                            ((between ws ws (pchar '=')) >>. pint32 .>> ts )
                            (fun value' num -> {Name = value'; Num = num}))))
                (fun name values -> EnumLock {Name = name; Values = values})

    let messageField =
        pstring "field" >>. ws1 >>.
        pipe3
            (identifier .>> ws1)
            (fullType')
            ((between ws ws (pchar '=')) >>. pint32 .>> ts )
            (fun name type' num -> Field {Name = name; Type = type'; Num = num})

    let messageOneOfCase =
        pstring "case" >>. ws1 >>.
        pipe2
            (identifier .>> ws1)
            ((between ws ws (pchar '=')) >>. pint32 .>> ts )
            (fun name num -> {CaseName = name; Num = num})

    let messageOneOf =
        pstring "oneof" >>. ws1 >>.
        pipe3
            (identifier .>> ws1)
            (complexName .>> ts)
            (many1 (ws >>. messageOneOfCase))
            (fun name unionName cases -> OneOf (name, unionName, cases))

    let messageLock =
        keyword "message" >>.
            pipe2
                (complexName .>> ts)
                (many (ws >>. choice [messageField; messageOneOf]))
                (fun name items -> MessageLock {Name = name; LockItems = items})

    let lockDocument =
        spaces >>. many (choice [enumLock; messageLock] .>> spaces) .>> eof

let parsePgenDoc input =
    match run Impl.pgenDocument input with
    | Success (model,_,_) -> Result.Ok model
    | Failure (err,_,_) -> Result.Error err

let parseLockDoc input =
    match run Impl.lockDocument input with
    | Success (model,_,_) -> Result.Ok model
    | Failure (err,_,_) -> Result.Error err