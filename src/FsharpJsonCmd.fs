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

    let rec genItem =
        function
        | Enum info ->
            let fullNameTxt = info.Name |> dottedName
            line txt $"    static member {firstName info.Name}FromString ="
            line txt $"        function"

            for symbol in info.Symbols do
                line txt $"        | \"{firstName info.Name}{symbol}\" -> {fullNameTxt}.{symbol}"

            line txt $"        | _ -> {fullNameTxt}.Unknown"
            line txt $""

            line txt $"    static member {firstName info.Name}ToString ="
            line txt $"        function"

            for symbol in info.Symbols do
                line txt $"        | {fullNameTxt}.{symbol} -> \"{firstName info.Name}{symbol}\""

            line txt $"        | _ -> \"Unknown\""
            line txt $""

        | Record info ->
            let fullNameTxt = info.Name |> dottedName

            line txt $"    static member {firstName info.Name}FromJson(reader: byref<Utf8JsonReader>): {fullNameTxt} ="
            readObject "v" info

            line txt $"        {{"

            for fieldInfo in info.Fields do
                match fieldInfo.Type with
                | Types.IsUnion typesCache _ -> line txt $"            {fieldInfo.Name} = v{fieldInfo.Name}"
                | Array _ -> line txt $"            {fieldInfo.Name} = v{fieldInfo.Name} |> Array.ofSeq"
                | List _ -> line txt $"            {fieldInfo.Name} = v{fieldInfo.Name} |> List.ofSeq"
                | Map _ -> line txt $"            {fieldInfo.Name} = v{fieldInfo.Name} |> Map.ofSeq"
                | _ -> line txt $"            {fieldInfo.Name} = v{fieldInfo.Name}"

            line txt $"        }}"

            line
                txt
                $"    static member {firstName info.Name}ToJson (writer: inref<Utf8JsonWriter>, x: {fullNameTxt}) ="

            writeObject "x." info
            line txt $""

        | Union union ->
            line
                txt
                $"    static member {firstName union.Name}FromJson(reader: byref<Utf8JsonReader>): {dottedName union.Name} ="

            line txt $"        let mutable y = {dottedName union.Name}.Unknown"

            line txt $"        if {helpers}.moveToStartObject(&reader)then"

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
                    match fieldInfo.Type with
                    | Array _
                    | List _
                    | Map _ -> linei txt 5 $"()"
                    | Complex t ->
                        linei
                            txt
                            5
                            $"y <- Convert{solidName ns}.{firstName t}FromJson(&reader) |> {dottedName union.Name}.{firstName case.Name}"
                    | t -> readValueBlock txt 5 typesCache t $"y <- v |> {dottedName union.Name}.{firstName case.Name}"
                | Types.MultiFieldsRecord ->
                    linei
                        txt
                        5
                        $"y <- Convert{solidName ns}.{firstName union.Name}Case{firstName case.Name}FromJson(&reader)"

            linei txt 4 $"else reader.Skip()"
            line txt $"        y"

            line
                txt
                $"    static member {firstName union.Name}ToJson (writer:inref<Utf8JsonWriter>, x: {dottedName union.Name}) ="

            line txt $"        writer.WriteStartObject()"
            line txt $"        match x with"

            for case in union.Cases do
                let values = case.Fields |> List.map Common.getName |> String.concat ","

                linei
                    txt
                    2
                    ($"| {dottedName case.Name}"
                     + (if values <> "" then $" ({values})" else "")
                     + " ->")

                linei txt 3 $"writer.WritePropertyName(\"{firstName case.Name}\")"

                match case with
                | Types.EmptyRecord -> linei txt 3 $"writer.WriteBooleanValue(true)"
                | Types.SingleFieldRecord fieldInfo -> linei txt 3 (writeValue typesCache values fieldInfo.Type)
                | Types.MultiFieldsRecord ->
                    linei
                        txt
                        3
                        $"Convert{lastNames union.Name |> solidName}.{firstName union.Name}Case{firstName case.Name}ToJson(&writer,{values})"

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
            match fieldInfo.Type with
            | Types.IsEnum typesCache enumInfo ->
                line txt $"        let mutable {prefix}{fieldInfo.Name} = {dottedName enumInfo.Name}.Unknown"
            | Types.IsUnion typesCache unionInfo ->
                line txt $"        let mutable {prefix}{fieldInfo.Name} = {dottedName unionInfo.Name}.Unknown"
            | _ ->
                line txt $"        let mutable {prefix}{fieldInfo.Name} = {FsharpTypesCmd.defValue true fieldInfo.Type}"

        line txt $"        if {helpers}.moveToStartObject(&reader) then"

        line txt $"            while {helpers}.moveToEndObject(&reader) = false do"
        line txt $"                if reader.TokenType <> JsonTokenType.PropertyName then ()"

        for fieldInfo in recordInfo.Fields do
            let vName = prefix + fieldInfo.Name

            let suffix =
                match fieldInfo.Type with
                | Optional _ -> "Value"
                | _ -> ""

            linei txt 4 $"else if (reader.ValueTextEquals(\"{firstCharToUpper fieldInfo.Name}{suffix}\")) then"

            match fieldInfo.Type with
            | Array t
            | List t ->
                linei txt 5 $"if reader.Read() && reader.TokenType = JsonTokenType.StartArray then"
                linei txt 6 $"while reader.Read() && reader.TokenType <> JsonTokenType.EndArray do"
                readValueBlock txt 7 typesCache t $"{vName}.Add(v)"
                linei txt 5 $"else reader.Skip()"
            | Map t ->
                linei txt 5 $"if reader.Read() && reader.TokenType = JsonTokenType.StartObject then"
                linei txt 6 $"while {helpers}.moveToEndObject(&reader) = false do"
                linei txt 7 $"let propName = reader.GetString()"
                readValueBlock txt 7 typesCache t $"{vName}.Add(propName, v)"
                linei txt 5 $"else reader.Skip()"
            | Types.IsRecord typesCache info ->
                let typeName = info.Name
                linei txt 5 $"{vName} <- Convert{lastNames typeName |> solidName}.{firstName typeName}FromJson(&reader)"
            | Types.IsUnion typesCache info ->
                let typeName = info.Name
                linei txt 5 $"{vName} <- Convert{lastNames typeName |> solidName}.{firstName typeName}FromJson(&reader)"
            | t -> readValueBlock txt 5 typesCache t $"{vName} <- v"

        line txt $"                else reader.Skip()"

    and writeObject prefix recordInfo =
        line txt $"        writer.WriteStartObject()"

        for fieldInfo in recordInfo.Fields do
            let vName = $"{prefix}{fieldInfo.Name}"

            match fieldInfo.Type with
            | Optional t ->
                let inner = "v"
                linei txt 2 $"match {vName} with"
                linei txt 2 $"| ValueSome v ->"
                linei txt 3 $"writer.WritePropertyName(\"{firstCharToUpper fieldInfo.Name}Value\")"
                linei txt 3 $"{writeValue typesCache inner t}"
                linei txt 2 $"| ValueNone -> ()"
            | _ ->
                let inner = $"{vName}"
                linei txt 2 $"writer.WritePropertyName(\"{firstCharToUpper fieldInfo.Name}\")"
                linei txt 2 $"{writeValue typesCache inner fieldInfo.Type}"

        line txt $"        writer.WriteEndObject()"

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
    | Int -> $"{helpers}.readInt(&reader)"
    | Long -> $"{helpers}.readLong(&reader)"
    | Float -> $"{helpers}.readSingle(&reader)"
    | Double -> $"{helpers}.readDouble(&reader)"
    | Money _ -> $"{helpers}.readMoney(&reader)"
    | Bytes -> $"{helpers}.readNytes(&reader)"
    | Timestamp -> $"{helpers}.readTimestamp(&reader)"
    | Duration -> $"{helpers}.readDuration(&reader)"
    | Guid -> $"{helpers}.readGuid(&reader)"
    | Optional t -> $"{readValue typesCache t} |> ValueOption.map ValueSome"
    | Types.IsEnum typesCache ei ->
        $"{helpers}.readString(&reader) |> ValueOption.map Convert{lastNames ei.Name |> solidName}.{firstName ei.Name}FromString"
    | Complex _
    | Array _
    | List _
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
        | Int
        | Long
        | Float
        | Double
        | Money _ -> $"writer.WriteNumberValue({vName})"
        | Bytes -> $"writer.WriteBase64StringValue(System.ReadOnlySpan({vName}))"
        | Timestamp -> $"{helpers}.writeTimestamp(&writer, {vName})"
        | Duration -> $"{helpers}.writeDuration(&writer, {vName})"
        | Guid -> $"{helpers}.writeGuid(&writer, {vName})"
        | Optional _ -> failwith "cannot unpack optional field"
        | Array t
        | List t ->
            let inner = "v"
            $"writer.WriteStartArray(); (for v in {vName} do {writeValue typesCache inner t}); writer.WriteEndArray()"
        | Map t ->
            let inner = "pair.Value"
            $"writer.WriteStartObject(); (for pair in {vName} do writer.WritePropertyName(pair.Key); {writeValue typesCache inner t}); writer.WriteEndObject()"
        | Complex typeName ->
            match typesCache.TryFind typeName with
            | Some(Enum _) ->
                $"writer.WriteStringValue({vName} |> Convert{lastNames typeName |> solidName}.{firstName typeName}ToString)"
            | _ -> $"Convert{lastNames typeName |> solidName}.{firstName typeName}ToJson(&writer, {vName})"

    f vName type'
