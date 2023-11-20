[<RequireQualifiedAccess>]
module rec Protokeep.FsharpJsonCmd

open System.Text
open Types
open Codegen

let helpers = "FsharpJsonHelpers"

let Handler module' locks typesCache =
    function
    | "-o" :: outputFileName :: args
    | "--output" :: outputFileName :: args ->
        Infra.checkLock module' locks typesCache
        |> Result.bind
           ^ fun _ ->
               let ns = Infra.checkArgNamespace args |> Option.defaultValue "Protokeep.FsharpJson"

               let fileName =
                   gen ns module' locks typesCache |> Infra.writeFile outputFileName ".fs"

               FsharpHelpers.update helpers |> Infra.updateCommons fileName args
    | x -> Error $"expected arguments [-o|--output] outputFile, but {x}"

let Instance =
    { Name = "fsharp-json"
      Description = "generate converters between json and fsharp types"
      Run = Handler }

let gen genNamespace (module': Module) (locks: LocksCollection) (typesCache: Types.TypesCache) =

    let txt = StringBuilder()
    let ns = module'.Name
    let genNs = genNamespace.Split('.') |> Seq.rev |> List.ofSeq |> ComplexName

    let rec genItem =
        function
        | Enum info ->
            let fullNameTxt = info.Name |> dottedName
            line txt $"    static member {firstName info.Name}FromString ="
            line txt $"        function"

            for symbol in info.Symbols do
                line txt $"        | \"{symbol}\" -> {fullNameTxt}.{symbol}"

            line txt $"        | _ -> {fullNameTxt}.Unknown"
            line txt $""

            line txt $"    static member {firstName info.Name}ToString ="
            line txt $"        function"

            for symbol in info.Symbols do
                line txt $"        | {fullNameTxt}.{symbol} -> \"{symbol}\""

            line txt $"        | _ -> \"Unknown\""
            line txt $""

        | Record info ->
            let fullNameTxt = info.Name |> dottedName

            line
                txt
                $"    static member {firstName info.Name}ToJson (writer: inref<Utf8JsonWriter>, x: {fullNameTxt}) ="

            writeObject "x." info
            line txt $""

            line
                txt
                $"    static member {firstName info.Name}FromJson(reader: byref<Utf8JsonReader>): {fullNameTxt} voption ="

            readObject "v" info

            line txt $"            ValueSome {{"

            for fieldInfo in info.Fields do
                let valueName = $"v{fieldInfo.Name}{FsharpTypesCmd.fromMutable fieldInfo.Type}"
                linei txt 4 $"{fieldInfo.Name} = {valueName}"

            line txt $"            }}"
            line txt $"        else ValueNone"


        | Union union ->
            line
                txt
                $"    static member {firstName union.Name}FromJson(reader: byref<Utf8JsonReader>): {dottedName union.Name} voption ="

            line txt $"        if {helpers}.moveToStartObject(&reader)then"
            line txt $"            let mutable y = {dottedName union.Name}.Unknown"

            line txt $"            while {helpers}.moveToEndObject(&reader) = false do"
            line txt $"                if reader.TokenType <> JsonTokenType.PropertyName then ()"

            for case in union.Cases do
                linei txt 4 $"else if (reader.ValueTextEquals(\"{firstName case.Name}\")) then"

                match case with
                | Types.EmptyRecord ->
                    linei txt 5 $"if reader.Read() && reader.TokenType = JsonTokenType.True"
                    linei txt 5 $"then y <- {dottedName union.Name}.{firstName case.Name}"
                    linei txt 5 $"else reader.Skip()"
                | Types.SingleFieldRecord fieldInfo ->
                    let prefix = "_"
                    declareFieldValue 5 prefix fieldInfo
                    readFieldValue 5 prefix fieldInfo
                    let ctr = $"|> {dottedName union.Name}.{firstName case.Name}"
                    linei txt 5 $"y <- {prefix}{fieldInfo.Name}{FsharpTypesCmd.fromMutable fieldInfo.Type} {ctr}"
                | Types.MultiFieldsRecord ->
                    let methodName = $"{firstName union.Name}Case{firstName case.Name}FromJson"
                    linei txt 5 $"y <- Convert{solidName ns}.{methodName}(&reader)"

            linei txt 4 $"else reader.Skip()"
            line txt $"            ValueSome y"
            line txt $"        else ValueNone"

            line
                txt
                $"    static member {firstName union.Name}ToJson (writer:inref<Utf8JsonWriter>, x: {dottedName union.Name}) ="

            line txt $"        writer.WriteStartObject()"
            line txt $"        match x with"

            for case in union.Cases do
                let values = case.Fields |> List.map getName |> String.concat ","

                linei
                    txt
                    2
                    ($"| {dottedName case.Name}"
                     + (if values <> "" then $" ({values})" else "")
                     + " ->")

                linei txt 3 $"writer.WritePropertyName(\"{firstName case.Name}\")"

                match case with
                | Types.EmptyRecord -> linei txt 3 $"writer.WriteBooleanValue(true)"
                | Types.SingleFieldRecord fieldInfo -> writeFieldValue 3 (getName fieldInfo) fieldInfo.Type
                | Types.MultiFieldsRecord ->
                    let methodName = $"{firstName union.Name}Case{firstName case.Name}ToJson"
                    linei txt 3 $"Convert{solidName ns}.{methodName}(&writer,{values})"

            line txt $"        | _ ->"
            line txt $"            writer.WritePropertyName(\"Unknown\")"
            line txt $"            writer.WriteBooleanValue(true)"
            line txt $"        writer.WriteEndObject()"

            for case in union.Cases do
                let fieldsNames =
                    case.Fields |> List.map (fun field -> field.Name) |> String.concat ","

                match case with
                | Types.MultiFieldsRecord ->
                    line
                        txt
                        $"    static member {firstName union.Name}Case{firstName case.Name}FromJson(reader: byref<Utf8JsonReader>) ="

                    readObject "" case

                    let convertedValues =
                        case.Fields
                        |> Seq.map (fun fieldInfo ->
                            match fieldInfo.Type with
                            | Array _
                            | List _ -> $"{fieldInfo.Name} |> List.ofSeq"
                            | Map _ -> $"{fieldInfo.Name} |> Map.ofSeq"
                            | _ -> fieldInfo.Name)
                        |> String.concat ","

                    line txt $"        {case.Name |> dottedName} ({convertedValues})"

                    line
                        txt
                        $"    static member {firstName union.Name}Case{firstName case.Name}ToJson (writer: inref<Utf8JsonWriter>,{fieldsNames}) ="

                    writeObject "" case
                | _ -> ()

            line txt $""

    and readObject prefix recordInfo =
        for fieldInfo in recordInfo.Fields do
            declareFieldValue 2 prefix fieldInfo

        line txt $"        if {helpers}.moveToStartObject(&reader) then"

        line txt $"            while {helpers}.moveToEndObject(&reader) = false do"
        line txt $"                if reader.TokenType <> JsonTokenType.PropertyName then ()"

        for fieldInfo in recordInfo.Fields do
            let suffix =
                match fieldInfo.Type with
                | Optional _ -> "Value"
                | _ -> ""

            linei txt 4 $"else if (reader.ValueTextEquals(\"{firstCharToUpper fieldInfo.Name}{suffix}\")) then"
            readFieldValue 5 prefix fieldInfo

        line txt $"                else reader.Skip()"

    and declareFieldValue ident prefix fieldInfo =
        let value = FsharpTypesCmd.defValue typesCache genNs true fieldInfo.Type
        linei txt ident $"let mutable {prefix}{fieldInfo.Name} = {value}"

    and readFieldValue ident prefix fieldInfo =
        let vName = $"{prefix}{fieldInfo.Name}"

        match fieldInfo.Type with
        | Array t
        | List t
        | Set t ->
            linei txt ident $"if reader.Read() && reader.TokenType = JsonTokenType.StartArray then"
            linei txt (ident + 1) $"while reader.TokenType <> JsonTokenType.EndArray do"
            readValueBlock txt (ident + 2) typesCache t $"{vName}.Add(v)"
            linei txt ident $"else reader.Skip()"
        | Map(k, v) ->
            linei txt ident $"if reader.Read() && reader.TokenType = JsonTokenType.StartArray then"
            linei txt (ident + 1) $"while reader.Read() && reader.TokenType <> JsonTokenType.EndArray do"
            linei txt (ident + 2) $"if reader.TokenType = JsonTokenType.StartObject then"
            linei txt (ident + 3) $"let mutable key = {FsharpTypesCmd.defValue typesCache genNs true k}"
            linei txt (ident + 3) $"let mutable value = {FsharpTypesCmd.defValue typesCache genNs true v}"
            linei txt (ident + 3) $"while reader.Read() && reader.TokenType <> JsonTokenType.EndObject do"
            linei txt (ident + 4) $"""if (reader.ValueTextEquals("Key")) then"""
            readValueBlock txt (ident + 5) typesCache k $"key <- v"
            linei txt (ident + 4) $"""else if (reader.ValueTextEquals("Value")) then"""
            readValueBlock txt (ident + 5) typesCache v $"value <- v"
            linei txt (ident + 4) $"else reader.Skip()"
            linei txt (ident + 3) $"{vName}.Add(key, value)"
            linei txt (ident + 2) $"else reader.Skip()"
            linei txt ident $"else reader.Skip()"
        | t -> readValueBlock txt 5 typesCache t $"{vName} <- v"

    and writeObject prefix recordInfo =
        line txt $"        writer.WriteStartObject()"

        for fieldInfo in recordInfo.Fields do
            let vName = $"{prefix}{fieldInfo.Name}"

            match fieldInfo.Type with
            | Optional t ->
                linei txt 2 $"match {vName} with"
                linei txt 2 $"| ValueSome v ->"
                linei txt 3 $"writer.WritePropertyName(\"{firstCharToUpper fieldInfo.Name}Value\")"
                writeFieldValue 3 "v" t
                linei txt 2 $"| ValueNone -> ()"
            | t ->
                linei txt 2 $"writer.WritePropertyName(\"{firstCharToUpper fieldInfo.Name}\")"
                writeFieldValue 2 vName t

        line txt $"        writer.WriteEndObject()"

    and writeFieldValue ident vName =
        function
        | Optional _ -> failwith "cannot unpack optional field"
        | Array t
        | List t
        | Set t ->
            linei txt ident $"writer.WriteStartArray()"
            linei txt ident $"for v in {vName} do"
            linei txt (ident + 1) $"""{writeValue typesCache "v" t}"""
            linei txt ident $"writer.WriteEndArray()"
        | Map(k, v) ->
            linei txt ident $"writer.WriteStartArray()"
            linei txt ident $"for pair in {vName} do"
            linei txt (ident + 1) $"writer.WriteStartObject()"
            linei txt (ident + 1) $"writer.WritePropertyName(\"Key\")"
            linei txt (ident + 1) $"""{writeValue typesCache "pair.Key" k}"""
            linei txt (ident + 1) $"writer.WritePropertyName(\"Value\")"
            linei txt (ident + 1) $"""{writeValue typesCache "pair.Value" v}"""
            linei txt (ident + 1) $"writer.WriteEndObject()"
            linei txt ident $"writer.WriteEndArray()"
        | t -> linei txt ident $"{writeValue typesCache vName t}"

    line txt $"namespace {genNamespace}"
    line txt $""
    line txt $"open System.Text.Json"
    line txt $"open Protokeep"
    line txt $""
    line txt $"type Convert{solidName module'.Name}() ="
    line txt $""

    for item in module'.Items do
        genItem item

    txt.ToString()


let rec readValue (typesCache: Types.TypesCache) =
    function
    | Bool -> $"{helpers}.readBoolean(&reader)"
    | String -> $"{helpers}.readString(&reader)"
    | Int8 -> $"{helpers}.readByte(&reader)"
    | Int16 -> $"{helpers}.readShort(&reader)"
    | Int32 -> $"{helpers}.readInt(&reader)"
    | Int64 -> $"{helpers}.readLong(&reader)"
    | Float32 -> $"{helpers}.readSingle(&reader)"
    | Float64 -> $"{helpers}.readDouble(&reader)"
    | Money scale -> $"{helpers}.readMoney(&reader, {scale})"
    | Binary -> $"{helpers}.readBytes(&reader)"
    | Timestamp -> $"{helpers}.readTimestamp(&reader)"
    | Duration -> $"{helpers}.readDuration(&reader)"
    | Guid -> $"{helpers}.readGuid(&reader)"
    | Optional t -> $"{readValue typesCache t} |> ValueOption.map ValueSome"
    | Types.IsEnum typesCache ei ->
        $"{helpers}.readString(&reader) |> ValueOption.map Convert{lastNames ei.Name |> solidName}.{firstName ei.Name}FromString"
    | Complex t -> $"Convert{lastNames t |> solidName}.{firstName t}FromJson(&reader)"
    | Array _
    | List _
    | Set _
    | Map _ -> failwith $"Collection in {nameof (readValue)}"

let rec readValueBlock txt ident (typesCache: Types.TypesCache) (typ: Type) expr =
    linei txt ident $"match {readValue typesCache typ} with"
    linei txt ident $"| ValueSome v -> {expr}"
    linei txt ident $"| ValueNone -> ()"

let rec writeValue (typesCache: Types.TypesCache) vName type' =
    let rec f vName =
        function
        | Bool -> $"writer.WriteBooleanValue({vName})"
        | String -> $"writer.WriteStringValue({vName})"
        | Int8
        | Int16
        | Int32
        | Int64
        | Float32
        | Float64
        | Money _ -> $"writer.WriteNumberValue({vName})"
        | Binary -> $"writer.WriteBase64StringValue(System.ReadOnlySpan({vName}))"
        | Timestamp -> $"{helpers}.writeTimestamp(&writer, {vName})"
        | Duration -> $"{helpers}.writeDuration(&writer, {vName})"
        | Guid -> $"{helpers}.writeGuid(&writer, {vName})"
        | Optional _ -> failwith "cannot unpack optional field"
        | Array _
        | List _
        | Set _
        | Map _ -> failwith "cannot unpack collection field"
        | Complex typeName ->
            match typesCache.TryFind typeName with
            | Some(Enum _) ->
                $"writer.WriteStringValue({vName} |> Convert{lastNames typeName |> solidName}.{firstName typeName}ToString)"
            | _ -> $"Convert{lastNames typeName |> solidName}.{firstName typeName}ToJson(&writer, {vName})"

    f vName type'
