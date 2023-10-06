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
                    | _ ->
                        linei txt 5 $"if reader.Read() && {checkTokenType typesCache fieldInfo.Type}"

                        linei
                            txt
                            5
                            $"then y <- {getValue typesCache fieldInfo.Type} |> {dottedName union.Name}.{firstName case.Name}"

                        linei txt 5 $"else reader.Skip()"
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
                | Types.SingleFieldRecord fieldInfo -> linei txt 3 (setValue typesCache values fieldInfo.Type)
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
                linei txt 7 $"if {checkTokenType typesCache t} then"
                linei txt 8 $"{vName}.Add({getValue typesCache t})"
                linei txt 7 $"else reader.Skip()"
                linei txt 5 $"else reader.Skip()"
            | Map t ->
                linei txt 5 $"if reader.Read() && reader.TokenType = JsonTokenType.StartObject then"
                linei txt 6 $"while {helpers}.moveToEndObject(&reader) = false do"
                linei txt 7 $"let propName = reader.GetString()"
                linei txt 7 $"if reader.Read() && {checkTokenType typesCache t} then"
                linei txt 8 $"{vName}.Add((propName, {getValue typesCache t}))"
                linei txt 7 $"else reader.Skip()"
                linei txt 5 $"else reader.Skip()"
            | Types.IsRecord typesCache _
            | Types.IsUnion typesCache _ -> linei txt 5 $"{vName} <- {getValue typesCache fieldInfo.Type}"
            | t ->
                linei txt 5 $"if reader.Read() && {checkTokenType typesCache t}"
                linei txt 5 $"then {vName} <- {getValue typesCache t}"
                linei txt 5 $"else reader.Skip()"

        line txt $"                else reader.Skip()"

    and writeObject prefix recordInfo =
        line txt $"        writer.WriteStartObject()"

        for fieldInfo in recordInfo.Fields do
            let vName = $"{prefix}{fieldInfo.Name}"

            match fieldInfo.Type with
            | Optional t ->
                let inner = "v"
                linei txt 2 $"match {vName} with"
                linei txt 2 $"| Some v ->"
                linei txt 3 $"writer.WritePropertyName(\"{firstCharToUpper fieldInfo.Name}Value\")"
                linei txt 3 $"{setValue typesCache inner t}"
                linei txt 2 $"| None -> ()"
            | _ ->
                let inner = $"{vName}"
                linei txt 2 $"writer.WritePropertyName(\"{firstCharToUpper fieldInfo.Name}\")"
                linei txt 2 $"{setValue typesCache inner fieldInfo.Type}"

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


let rec checkTokenType (typesCache: Types.TypesCache) =
    function
    | Bool -> "(reader.TokenType = JsonTokenType.True || reader.TokenType = JsonTokenType.False)"
    | String
    | Bytes
    | Timestamp
    | Duration
    | Guid -> "reader.TokenType = JsonTokenType.String"
    | Int
    | Long
    | Float
    | Double
    | Decimal _ -> "reader.TokenType = JsonTokenType.Number"
    | Optional x -> checkTokenType typesCache x
    | Types.IsEnum typesCache _ -> "reader.TokenType = JsonTokenType.String"
    | Complex _ -> "reader.TokenType = JsonTokenType.StartObject"
    | Array _
    | List _
    | Map _ -> failwith $"Collection in {nameof (checkTokenType)}"

let rec getValue (typesCache: Types.TypesCache) =
    function
    | Bool -> "reader.GetBoolean()"
    | String -> "reader.GetString()"
    | Int -> "reader.GetInt32()"
    | Long -> "reader.GetInt64()"
    | Float -> "reader.GetSingle()"
    | Double -> "reader.GetDouble()"
    | Decimal scale -> $"decimal(float(reader.GetInt64()) / {10. ** float (scale)}.)"
    | Bytes -> "reader.GetBytesFromBase64()"
    | Timestamp -> "reader.GetDateTime()"
    | Duration -> $"reader.GetString() |> {helpers}.toTimeSpan"
    | Guid -> "System.Guid(reader.GetBytesFromBase64())"
    | Optional t -> $"{getValue typesCache t} |> Some"
    | Types.IsEnum typesCache ei ->
        $"reader.GetString() |> Convert{lastNames ei.Name |> solidName}.{firstName ei.Name}FromString"
    | Complex typeName -> $"Convert{lastNames typeName |> solidName}.{firstName typeName}FromJson(&reader)"
    | Array _
    | List _
    | Map _ -> failwith $"Collection in {nameof (getValue)}"

let rec setValue (typesCache: Types.TypesCache) vName type' =
    let rec f vName =
        function
        | Bool -> $"writer.WriteBooleanValue({vName})"
        | String -> $"writer.WriteStringValue({vName})"
        | Int
        | Long
        | Float
        | Double -> $"writer.WriteNumberValue({vName})"
        | Decimal scale -> $"writer.WriteNumberValue({vName} * {10. ** float (scale)}m |> System.Decimal.Truncate)"
        | Bytes -> $"writer.WriteBase64StringValue(System.ReadOnlySpan({vName}))"
        | Timestamp -> $"writer.WriteStringValue({vName} |> {helpers}.fromDateTime)"
        | Duration -> $"writer.WriteStringValue({vName} |> {helpers}.fromTimeSpan)"
        | Guid -> $"writer.WriteBase64StringValue(System.ReadOnlySpan({vName}.ToByteArray()))"
        | Optional _ -> failwith "cannot unpack optional field"
        | Array t
        | List t ->
            let inner = "v"
            $"writer.WriteStartArray(); (for v in {vName} do {setValue typesCache inner t}); writer.WriteEndArray()"
        | Map t ->
            let inner = "pair.Value"
            $"writer.WriteStartObject(); (for pair in {vName} do writer.WritePropertyName(pair.Key); {setValue typesCache inner t}); writer.WriteEndObject()"
        | Complex typeName ->
            match typesCache.TryFind typeName with
            | Some(Enum _) ->
                $"writer.WriteStringValue({vName} |> Convert{lastNames typeName |> solidName}.{firstName typeName}ToString)"
            | _ -> $"Convert{lastNames typeName |> solidName}.{firstName typeName}ToJson(&writer, {vName})"

    f vName type'
