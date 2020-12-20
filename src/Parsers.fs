[<RequireQualifiedAccess>]
module Protogen.Parsers

open FParsec
open Types

module private Impl =

    let ws<'u> : Parser<_,'u> = skipMany (skipAnyOf " \t")
    let ws1<'u> : Parser<_,'u> = skipMany1 (skipAnyOf " \t")
    let ts<'u> : Parser<_,'u> = ws >>. (skipNewline <|> eof)  // trailing spaces
    let keyword name = pstring name >>. ws1

    let boolOfOpt = Option.map (fun _ -> true) >> Option.defaultValue false

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
            (fun name symbols -> {| Name = name; Symbols = symbols |})

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
            ws >>?
            opt (choice[
                    skipString "option" |>> (fun () -> Optional t)
                    skipString "array" |>> (fun () -> Array t)
                    skipString "map" |>> (fun () -> Map t)])
            |>> (Option.defaultValue t))

    let indexKey' =
        choice[
            skipChar '.' >>. identifier |>> IndexKey.FieldKey
        ]

    let indexValue' =
        choice[
            skipChar '.' >>. identifier |>> IndexValue.Field
        ]

    let indexDefinition' =
        pipe2
            (opt (indexKey' .>>? (between ws ws (skipString "=>"))))
            indexValue'
            (fun ikey ival -> (ikey |> Option.defaultValue Num, ival))

    let index' =
        skipString "idx" >>.
        pipe2
            (opt (skipChar ':' >>. identifier))
            (opt (between (skipChar '[' .>> ws) (ws >>. skipChar ']') indexDefinition'))
            (fun idxName def ->
                {
                    Name = idxName |> Option.defaultValue "item"
                    Key = def |> Option.map fst |> Option.defaultValue Num
                    Value = def |> Option.map snd |> Option.defaultValue IndexValue.Self
                })
    let field' =
        pipe4
            ((between spaces ws identifier) .>> pchar ':')
            (between ws ws fullType')
            (opt (skipString "key") .>> ws)
            (many (index' .>> ws))
            (fun name type' isKey idxs -> {|Name = name; Type = type'; IsKey = isKey; Indexes = idxs|})

    let record' =
        keyword "record" >>.
        pipe2
            (identifier .>> ws .>> skipChar '=' .>> spaces)
            (between (pchar '{') (pchar '}' .>> spaces)
                (sepEndBy field' (pchar ';' <|> newline)))
            (fun name fields -> {| Name = name; Fields = fields |})

    let unionCaseField' =
        pipe3
            (opt ((between ws ws identifier) .>>? pchar ':'))
            (between ws ws fullType')
            (opt (skipString "key") .>> ws)
            (fun name type' isKey -> {|Name = name; Type = type'; IsKey = isKey|})

    let unionCase' =
        pipe2
            identifier
            (opt (ws1 .>>? (keyword "of") >>. sepBy unionCaseField' (pchar '*')))
            (fun name fields -> {| Name = name; Fields = fields |})

    let union' =
        keyword "union" >>.
        pipe2
            (identifier .>> ws .>> skipChar '=' .>> spaces .>> opt (pchar '|'))
            (sepBy1 (between spaces spaces unionCase') (pchar '|') )
            (fun name cases -> {| Name = name; Cases = cases|})

    let module' =
        keyword "module" >>.
            (complexName .>> ts)
                >>= (fun moduleName ->
                    (many (
                        spaces
                        >>. choice [
                            enum' |>> (fun r ->
                                Enum {
                                    Name = Types.mergeName moduleName r.Name
                                    Symbols = r.Symbols
                                })
                            record' |>> (fun r ->
                                Record {
                                    Name = Types.mergeName moduleName r.Name
                                    Fields =
                                        r.Fields
                                        |> List.map (fun fr ->
                                            {   Name = fr.Name
                                                Type = fr.Type
                                                IsKey = boolOfOpt fr.IsKey
                                                Indexes = fr.Indexes |> List.map (fun ir -> {Name = ir.Name; Key = ir.Key; Value = ir.Value})
                                            })
                                })
                            union' |>> (fun r ->
                                let unionName = Types.mergeName moduleName r.Name
                                Union {
                                    Name = unionName
                                    Cases =
                                        r.Cases
                                        |> List.map (fun cr ->
                                            {   Name = Types.mergeName unionName cr.Name
                                                Fields =
                                                    cr.Fields |> Option.map (fun fieldsList ->
                                                        fieldsList
                                                        |> List.mapi (fun idx fr ->
                                                            {   Name = fr.Name |> Option.defaultValue (sprintf "p%d" (idx + 1))
                                                                Type = fr.Type
                                                                IsKey = boolOfOpt fr.IsKey
                                                                Indexes = []
                                                            }
                                                        )
                                                    ) |> Option.defaultValue []
                                            })
                                })]
                    ) |>> (fun items -> {Name = moduleName; Items = items})
            ))

    let pgenDocument =
        spaces >>. module' .>> spaces .>> eof

    let enumLock =
        keyword "enum" >>.
        pipe2
            (complexName .>> ts)
            (many1 (
                (ws >>. pstring "value" >>. ws1 >>.
                    pipe2
                        identifier
                        ((between ws ws (pchar '=')) >>. pint32 .>> ts )
                        (fun value' num -> {Name = value'; Num = num} : EnumValueLock))))
            (fun name values -> EnumLock {Name = name; Values = values})

    let messageField =
        keyword "field" >>.
        pipe3
            (identifier .>> ws1)
            (fullType')
            ((between ws ws (pchar '=')) >>. pint32 .>> ts )
            (fun name type' num -> Field {Name = name; Type = type'; Num = num})

    let messageOneOfCase =
        keyword "case" >>.
        pipe2
            (identifier .>> ws1)
            ((between ws ws (pchar '=')) >>. pint32 .>> ts )
            (fun name num -> {CaseName = name; Num = num})

    let messageOneOf =
        keyword "oneof" >>.
        pipe3
            (identifier .>> ws1)
            (complexName .>> ts)
            (many1 (spaces >>. messageOneOfCase .>> spaces))
            (fun name unionName cases -> OneOf (name, unionName, cases))

    let messageLock =
        keyword "message" >>.
        pipe2
            (complexName .>> ts)
            (many (ws >>. choice [messageField; messageOneOf]))
            (fun name items -> MessageLock {Name = name; LockItems = items})

    let recordFieldLock =
        keyword "field" >>.
        pipe3
            (identifier .>> ws1)
            (between ws ws fullType')
            ((between ws ws (pchar '=')) >>. pint32 .>> ts )
            (fun name type' num -> {Name = name; Type = type'; Num = num})

    let recordLock =
        keyword "record" >>.
        pipe2
            (complexName .>> ts)
            (many (ws >>. recordFieldLock))
            (fun name items -> RecordLock {Name = name; Fields = items})

    let unionCaseLock =
        keyword "case" >>.
        pipe2
            (identifier .>> ws1)
            ((between ws ws (pchar '=')) >>. pint32 .>> ts )
            (fun name num -> {Name = name; Num = num} : UnionCaseLock)

    let unionLock =
        keyword "union" >>.
        pipe2
            (complexName .>> ts)
            (many (ws >>. unionCaseLock))
            (fun name items -> UnionLock {Name = name; Cases = items})

    let lockDocument =
        spaces >>. many (choice [enumLock; recordLock; unionLock; messageLock] .>> spaces) .>> eof

    let fsharpCoreDocument =
        spaces >>. pstring "namespace" >>. ws1 >>. pstring "Protogen" >>. skipRestOfLine true >>.
        pstring "module" >>.
        many (
            pipe2
                (ws1 >>. identifier .>> ws1 .>> pchar '=' .>> ts)
                (many1Till
                    (restOfLine true)
                    (skipString "module" <|> eof))
                (fun moduleName moduleBody -> moduleName, moduleBody |> String.concat "\n")
            )
        .>> eof

let parsePgenDoc input =
    match run Impl.pgenDocument input with
    | Success (model,_,_) -> Result.Ok model
    | Failure (err,_,_) -> Result.Error err

let parseLockDoc input =
    match run Impl.lockDocument input with
    | Success (model,_,_) -> Result.Ok model
    | Failure (err,_,_) -> Result.Error err

let parseFsharpCoreDoc input =
    match run Impl.fsharpCoreDocument input with
    | Success (model,_,_) -> Result.Ok model
    | Failure (err,_,_) -> Result.Error err