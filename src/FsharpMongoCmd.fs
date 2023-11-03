[<RequireQualifiedAccess>]
module rec Protokeep.FsharpMongoCmd

open System.Text
open Types
open Codegen

let helpers = "FsharpMongoHelpers"

let Handler module' locks typesCache =
    function
    | "-o" :: outputFileName :: args
    | "--output" :: outputFileName :: args ->
        Infra.checkLock module' locks typesCache
        |> Result.bind
           ^ fun _ ->
               let ns = Infra.checkArgNamespace args |> Option.defaultValue "Protokeep.FsharpMongo"

               let fileName =
                   gen ns module' locks typesCache |> Infra.writeFile outputFileName ".fs"

               FsharpHelpers.update helpers |> Infra.updateCommons fileName args
    | x -> Error $"expected arguments [-o|--output] outputFile, but {x}"

let Instance =
    { Name = "fsharp-mongo"
      Description = "generate serializers for Mongo Driver"
      Run = Handler }

let gen genNamespace (module': Module) (locks: LocksCollection) (typesCache: Types.TypesCache) =

    let txt = StringBuilder()
    let ns = module'.Name
    let genNs = genNamespace.Split('.') |> Seq.rev |> List.ofSeq |> ComplexName

    let rec genItem =
        function
        | Enum info ->
            let fullNameTxt = info.Name |> dottedName
            line txt $"    static member{firstName info.Name}FromInt = function"

            for symbol in info.Symbols do
                line txt $"        | {fullNameTxt}.{symbol} -> {fullNameTxt}.{symbol}"

            line txt $"        | _ -> {fullNameTxt}.Unknown"
            line txt $""
        | Record info ->
            let fullNameTxt = info.Name |> dottedName

            let asEntity = if info.HasKey then ", ?asEntity: bool" else ""
            line txt $"    static member {firstName info.Name}ToBson(writer: IBsonWriter, x: {fullNameTxt}{asEntity}) ="

            writeObject "x." info
            line txt $""

            line txt $"    static member {firstName info.Name}FromBson(reader: IBsonReader): {fullNameTxt} ="
            readObject "v" info

            line txt $"        {{"

            for fieldInfo in info.Fields do
                match fieldInfo.Type with
                | Types.IsUnion typesCache _ -> line txt $"            {fieldInfo.Name} = v{fieldInfo.Name}"
                | Array _ -> line txt $"            {fieldInfo.Name} = v{fieldInfo.Name} |> Array.ofSeq"
                | List _ -> line txt $"            {fieldInfo.Name} = v{fieldInfo.Name} |> List.ofSeq"
                | Set _ -> line txt $"            {fieldInfo.Name} = v{fieldInfo.Name} |> Set.ofSeq"
                | Map _ -> line txt $"            {fieldInfo.Name} = v{fieldInfo.Name} |> Map.ofSeq"
                | _ -> line txt $"            {fieldInfo.Name} = v{fieldInfo.Name}"

            line txt $"        }}"
            line txt $""

        | Union union ->
            writeUnion union
            readUnion union

    and writeUnion union =
        line txt $"    static member {firstName union.Name}ToBson(writer: IBsonWriter, x: {dottedName union.Name}) ="

        line txt $"        writer.WriteStartDocument()"
        line txt $"        match x with"

        for case in union.Cases do
            let values = case.Fields |> List.map Common.getName |> String.concat ","

            linei
                txt
                2
                ($"| {dottedName case.Name}"
                 + (if values <> "" then $" ({values})" else "")
                 + " ->")

            linei txt 3 $"writer.WriteName(\"{firstName case.Name}\")"

            match case with
            | Types.EmptyRecord -> linei txt 3 $"writer.WriteBoolean(true)"
            | Types.SingleFieldRecord fieldInfo -> writeFieldValue 3 (getName fieldInfo) fieldInfo.Type
            | Types.MultiFieldsRecord ->
                let methodName = $"{firstName union.Name}Case{firstName case.Name}ToBson"
                linei txt 3 $"Convert{solidName ns}.{methodName}(writer,{values})"

        line txt $"        | _ ->"
        line txt $"            writer.WriteName(\"Unknown\")"
        line txt $"            writer.WriteBoolean(true)"
        line txt $"        writer.WriteEndDocument()"

        for case in union.Cases do
            let fieldsNames =
                case.Fields |> List.map (fun field -> field.Name) |> String.concat ","

            match case with
            | Types.MultiFieldsRecord ->
                line
                    txt
                    $"    static member {firstName union.Name}Case{firstName case.Name}ToBson (writer: IBsonWriter,{fieldsNames}) ="

                writeObject "" case
            | _ -> ()

    and readUnion union =
        line txt $"    static member {firstName union.Name}FromBson (reader: IBsonReader): {dottedName union.Name} ="
        line txt $"        let mutable y = {dottedName union.Name}.Unknown"
        line txt $"        reader.ReadStartDocument()"
        line txt $"        match reader.ReadName() with"

        for case in union.Cases do
            linei txt 4 $"| \"{firstName case.Name}\" ->"

            match case with
            | Types.EmptyRecord ->
                linei txt 5 $"match {helpers}.readBoolean reader with"
                linei txt 5 $"| ValueSome true -> y <- {dottedName union.Name}.{firstName case.Name}"
                linei txt 5 $"| _ -> ()"
            | Types.SingleFieldRecord fieldInfo ->
                let prefix = "_"
                declareFieldValue 5 prefix fieldInfo
                readFieldValue 5 prefix fieldInfo
                let ctr = $"|> {dottedName union.Name}.{firstName case.Name}"
                linei txt 5 $"y <- {prefix}{fieldInfo.Name}{FsharpTypesCmd.fromMutable fieldInfo.Type} {ctr}"
            | Types.MultiFieldsRecord ->
                let methodName = $"{firstName union.Name}Case{firstName case.Name}FromBson"
                linei txt 5 $"y <- Convert{solidName ns}.{methodName}(reader)"

        linei txt 4 $"| _ -> ()"
        line txt $"        reader.ReadEndDocument()"
        line txt $"        y"

        for case in union.Cases do
            match case with
            | Types.MultiFieldsRecord ->
                line
                    txt
                    $"    static member {firstName union.Name}Case{firstName case.Name}FromBson (reader: IBsonReader) ="

                readObject "" case

                let convertedValues =
                    case.Fields
                    |> Seq.map (fun fieldInfo ->
                        match fieldInfo.Type with
                        | Array _ -> $"{fieldInfo.Name} |> Array.ofSeq"
                        | List _ -> $"{fieldInfo.Name} |> List.ofSeq"
                        | Set _ -> $"{fieldInfo.Name} |> Set.ofSeq"
                        | Map _ -> $"{fieldInfo.Name} |> Map.ofSeq"
                        | _ -> fieldInfo.Name)
                    |> String.concat ","

                line txt $"        {case.Name |> dottedName} ({convertedValues})"
            | _ -> ()

    and readObject prefix recordInfo =
        for fieldInfo in recordInfo.Fields do
            declareFieldValue 2 prefix fieldInfo

        line txt $"        reader.ReadStartDocument()"

        line txt $"        while reader.State <> BsonReaderState.EndOfDocument do"
        line txt $"            match reader.State with"
        line txt $"            | BsonReaderState.Type -> reader.ReadBsonType() |> ignore"
        line txt $"            | BsonReaderState.Name ->"
        line txt $"                match reader.ReadName() with"

        for fieldInfo in recordInfo.Fields do
            let suffix =
                match fieldInfo.Type with
                | Optional _ -> "Value"
                | _ -> ""

            linei txt 4 $"| \"{firstCharToUpper fieldInfo.Name}{suffix}\" ->"
            readFieldValue 5 prefix fieldInfo

        line txt $"                | _ -> reader.SkipValue()"
        line txt "            | _ -> printfn \"Unexpected state: %A\" reader.State"
        line txt $"        reader.ReadEndDocument()"

    and declareFieldValue ident prefix fieldInfo =
        match fieldInfo.Type with
        | Types.IsEnum typesCache enumInfo ->
            linei txt ident $"let mutable {prefix}{fieldInfo.Name} = {dottedName enumInfo.Name}.Unknown"
        | Types.IsUnion typesCache unionInfo ->
            linei txt ident $"let mutable {prefix}{fieldInfo.Name} = {dottedName unionInfo.Name}.Unknown"
        | _ ->
            let value = FsharpTypesCmd.defValue genNs true fieldInfo.Type
            linei txt ident $"let mutable {prefix}{fieldInfo.Name} = {value}"

    and readFieldValue ident prefix (fieldInfo: FieldInfo) =
        let vName = prefix + fieldInfo.Name

        match fieldInfo.Type with
        | Array t
        | List t
        | Set t ->
            linei txt 5 $"reader.ReadStartArray()"
            linei txt 5 $"while reader.ReadBsonType() <> BsonType.EndOfDocument do"
            linei txt 6 $"match {readValue typesCache t} with"
            linei txt 6 $"| ValueSome v -> {vName}.Add(v)"
            linei txt 6 $"| ValueNone -> ()"
            linei txt 5 $"reader.ReadEndArray()"
        | Map(k, v) ->
            linei txt 5 $"reader.ReadStartArray()"
            linei txt 5 $"while reader.ReadBsonType() <> BsonType.EndOfDocument do"
            linei txt 6 $"reader.ReadStartDocument()"
            linei txt 6 $"let mutable key = {FsharpTypesCmd.defValue genNs true k}"
            linei txt 6 $"let mutable value = {FsharpTypesCmd.defValue genNs true v}"
            linei txt 6 $"while reader.ReadBsonType() <> BsonType.EndOfDocument do"
            linei txt 7 $"match reader.ReadName() with"
            linei txt 7 $"""| "Key" ->"""
            linei txt 8 $"match {readValue typesCache k} with"
            linei txt 8 $"| ValueSome v -> key <- v"
            linei txt 8 $"| ValueNone -> ()"
            linei txt 7 $"""| "Value" ->"""
            linei txt 8 $"match {readValue typesCache v} with"
            linei txt 8 $"| ValueSome v -> value <- v"
            linei txt 8 $"| ValueNone -> ()"
            linei txt 7 $"| _ -> reader.SkipValue()"
            linei txt 6 $"reader.ReadEndDocument()"
            linei txt 6 $"{vName}.Add(key, value)"
            linei txt 5 $"reader.ReadEndArray()"
        | t ->
            linei txt 5 $"match {readValue typesCache t} with"
            linei txt 5 $"| ValueSome v -> {vName} <- v"
            linei txt 5 $"| ValueNone -> ()"

    and writeObject prefix recordInfo =
        line txt $"        writer.WriteStartDocument()"

        if recordInfo.HasKey then
            line txt $"        if asEntity.IsSome then"
            line txt $"            {helpers}.writeId (writer, {prefix.TrimEnd '.'})"

        for fieldInfo in recordInfo.Fields do
            let vName = $"{prefix}{fieldInfo.Name}"

            match fieldInfo.Type with
            | Optional t ->
                let inner = "v"
                linei txt 2 $"match {vName} with"
                linei txt 2 $"| ValueSome v ->"
                linei txt 3 $"writer.WriteName(\"{firstCharToUpper fieldInfo.Name}Value\")"
                writeFieldValue 3 inner t
                linei txt 2 $"| ValueNone -> ()"
            | t ->
                linei txt 2 $"writer.WriteName(\"{firstCharToUpper fieldInfo.Name}\")"
                writeFieldValue 2 vName t

        line txt $"        writer.WriteEndDocument()"

    and writeFieldValue ident vName =
        function
        | Optional t -> failwith "cannot unpack optional field"
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
            linei txt (ident + 1) $"writer.WriteStartDocument()"
            linei txt (ident + 1) $"""writer.WriteName("Key")"""
            linei txt (ident + 1) $"""{writeValue typesCache "pair.Key" k}"""
            linei txt (ident + 1) $"""writer.WriteName("Value")"""
            linei txt (ident + 1) $"""{writeValue typesCache "pair.Value" v}"""
            linei txt (ident + 1) $"writer.WriteEndDocument()"
            linei txt ident $"writer.WriteEndArray()"
        | t ->
            let inner = $"{vName}"
            linei txt ident $"{writeValue typesCache inner t}"


    line txt $"namespace {genNamespace}"
    line txt $""
    line txt $"open MongoDB.Bson"
    line txt $"open MongoDB.Bson.IO"
    line txt $"open MongoDB.Bson.Serialization"
    line txt $"open MongoDB.Bson.Serialization.Serializers"
    line txt $"open Protokeep"
    line txt $""
    let className = $"Convert{solidName module'.Name}"
    line txt $"type {className}() ="

    for item in module'.Items do
        genItem item

    let serializableTypeNames =
        module'.Items
        |> Seq.choose
           ^ function
               | Record info when info.HasKey -> Some info.Name
               | _ -> None

    line txt $""

    for typeName in serializableTypeNames do
        line txt $"type {typeName |> firstName}Serializer() ="
        line txt $"    inherit SerializerBase<{typeName |> dottedName}>()"
        line txt $"    override x.Deserialize(ctx: BsonDeserializationContext, args: BsonDeserializationArgs) ="
        line txt $"        {className}.{typeName |> firstName}FromBson(ctx.Reader)"
        line txt $""

        line
            txt
            $"    override x.Serialize(ctx: BsonSerializationContext, args: BsonSerializationArgs, value: {typeName |> dottedName}) ="

        line txt $"        {className}.{typeName |> firstName}ToBson(ctx.Writer, value, true)"
        line txt $""


    line txt $"type {className} with"
    line txt $"    static member RegisterSerializers() ="
    line txt $"        BsonDefaults.GuidRepresentationMode <- GuidRepresentationMode.V3"

    for typeName in serializableTypeNames do
        line txt $"        BsonSerializer.RegisterSerializer({typeName |> firstName}Serializer())"

    txt.ToString()

let rec readValue (typesCache: Types.TypesCache) =
    function
    | Bool -> $"{helpers}.readBoolean reader"
    | String -> $"{helpers}.readString reader"
    | Int8 -> $"{helpers}.readInt8 reader"
    | Int16 -> $"{helpers}.readInt16 reader"
    | Int32 -> $"{helpers}.readInt32 reader"
    | Int64 -> $"{helpers}.readInt64 reader"
    | Float32 -> $"{helpers}.readFloat32 reader"
    | Float64 -> $"{helpers}.readFloat64 reader"
    | Money scale -> $"{helpers}.readMoney(reader, {scale})"
    | Binary -> $"{helpers}.readBinary reader"
    | Timestamp -> $"{helpers}.readTimestamp reader"
    | Duration -> $"{helpers}.readDuration reader"
    | Guid -> $"{helpers}.readGuid reader"
    | Optional t -> $"{readValue typesCache t} |> ValueOption.map ValueSome"
    | Types.IsEnum typesCache ei ->
        let readFn = readValue typesCache ei.Type
        $"{readFn} |> ValueOption.map (fun v -> LanguagePrimitives.EnumOfValue ({FsharpTypesCmd.primitiveTypeToString ei.Type} v))"
    | Complex typeName -> $"Convert{lastNames typeName |> solidName}.{firstName typeName}FromBson(reader) |> ValueSome"
    | Array _
    | List _
    | Set _
    | Map _ -> failwith $"Collection in {nameof (readValue)}"

let rec writeValue (typesCache: Types.TypesCache) vName type' =
    let rec f vName =
        function
        | Bool -> $"writer.WriteBoolean({vName})"
        | String -> $"writer.WriteString({vName})"
        | Int8 -> $"writer.WriteInt32(int {vName})"
        | Int16 -> $"writer.WriteInt32(int {vName})"
        | Int32 -> $"writer.WriteInt32({vName})"
        | Int64 -> $"writer.WriteInt64({vName})"
        | Float32 -> $"writer.WriteDouble(double {vName})"
        | Float64 -> $"writer.WriteDouble({vName})"
        | Money scale -> $"writer.WriteInt32({helpers}.toMoney ({vName}, {scale}))"
        | Binary -> $"writer.WriteBytes({vName})"
        | Timestamp -> $"writer.WriteDateTime({helpers}.fromDateTime {vName})"
        | Duration -> $"writer.WriteInt32({vName}.TotalMilliseconds |> int)"
        | Guid -> $"{helpers}.writeGuid(writer, {vName})"
        | Optional _ -> failwith "cannot unpack optional field"
        | Array _
        | List _
        | Set _
        | Map _ -> failwith "cannot unpack collection"
        | Complex typeName ->
            match typesCache.TryFind typeName with
            | Some(Enum _) -> $"writer.WriteInt32({vName} |> int)"
            | _ -> $"Convert{lastNames typeName |> solidName}.{firstName typeName}ToBson(writer, {vName})"

    f vName type'
