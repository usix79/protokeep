[<RequireQualifiedAccess>]
module rec Protokeep.FsharpMongoConvertersCmd

open System
open System.Text
open System.IO
open Types
open Codegen

let helpers = "FsharpMongoHelpers"

let Handler module' locks typesCache =
    function
    | "-o" :: outputFileName :: args
    | "--output" :: outputFileName :: args ->
        Program.checkLock module' locks typesCache
        |> Result.bind (fun _ ->
            let ns =
                Program.checkArgNamespace args |> Option.defaultValue "Protokeep.FsharpMongo"

            let fileContent = gen ns module' locks typesCache

            let fileName =
                if Path.GetExtension(outputFileName) <> ".fs" then
                    outputFileName + ".g.fs"
                else
                    outputFileName

            Console.WriteLine($"Writing fsharp mongo converters to {fileName}")
            File.WriteAllText(fileName, fileContent)

            let defaultCommonsFileName =
                Path.Combine(Path.GetDirectoryName(fileName), "Protokeep.fs")

            Program.checkArgUpdateCommons defaultCommonsFileName args
            |> Option.iter (fun coreFileName ->
                let coreFileText =
                    if (File.Exists coreFileName) then
                        File.ReadAllText(coreFileName)
                    else
                        ""

                let updatedCoreFileText = CoreFsharp.update coreFileText helpers moduleBody

                Console.WriteLine($"Adding FsharpMongo module to {coreFileName}")
                File.WriteAllText(coreFileName, updatedCoreFileText))

            Ok())
    | x -> Error $"expected arguments [-o|--output] outputFile, but {x}"

let Instance =
    { Name = "fsharp-mongo"
      Description = "generate converters and serializers for Mongo Driver: fsharp-mongo [-o|--output] outputFile"
      Run = Handler }

let gen genNamespace (module': Module) (locks: LocksCollection) (typesCache: Types.TypesCache) =

    let txt = StringBuilder()
    let ns = module'.Name

    let rec genItem =
        function
        | Enum info ->
            let fullNameTxt = info.Name |> dottedName

            line txt $"    static member {firstName info.Name}FromInt = function"

            for symbol in info.Symbols do
                line txt $"        | {fullNameTxt}.{symbol} -> {fullNameTxt}.{symbol}"

            line txt $"        | _ -> {fullNameTxt}.Unknown"

        | Record info ->
            let fullNameTxt = info.Name |> dottedName

            line txt $"    static member {firstName info.Name}ToBson (writer: IBsonWriter, x: {fullNameTxt}) ="
            writeObject "x." info

            line txt $"    static member {firstName info.Name}FromBson (reader: IBsonReader): {fullNameTxt} ="
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

        | Union union ->
            writeUnion union
            readUnion union

    and writeUnion union =
        line txt $"    static member {firstName union.Name}ToBson (writer: IBsonWriter, x: {dottedName union.Name}) ="

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
            | Types.SingleFieldRecord fieldInfo -> linei txt 3 (setValue typesCache values fieldInfo.Type)
            | Types.MultiFieldsRecord ->
                linei
                    txt
                    3
                    $"Convert{lastNames union.Name |> solidName}.{firstName union.Name}Case{firstName case.Name}ToBson(writer,{values})"

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
                match fieldInfo.Type with
                | Array _
                | List _
                | Map _ -> linei txt 5 $"()"
                | _ ->
                    linei txt 5 $"match {getValue typesCache fieldInfo.Type} with"
                    linei txt 5 $"| ValueSome v -> y <- v |> {dottedName union.Name}.{firstName case.Name}"
                    linei txt 5 $"| _ -> ()"
            | Types.MultiFieldsRecord ->
                linei
                    txt
                    5
                    $"y <- Convert{solidName ns}.{firstName union.Name}Case{firstName case.Name}FromBson(reader)"

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
                        | Array _
                        | List _ -> $"{fieldInfo.Name} |> List.ofSeq"
                        | Map _ -> $"{fieldInfo.Name} |> Map.ofSeq"
                        | _ -> fieldInfo.Name)
                    |> String.concat ","

                line txt $"        {case.Name |> dottedName} ({convertedValues})"
            | _ -> ()

    and readObject prefix recordInfo =
        for fieldInfo in recordInfo.Fields do
            match fieldInfo.Type with
            | Types.IsEnum typesCache enumInfo ->
                line txt $"        let mutable {prefix}{fieldInfo.Name} = {dottedName enumInfo.Name}.Unknown"
            | Types.IsUnion typesCache unionInfo ->
                line txt $"        let mutable {prefix}{fieldInfo.Name} = {dottedName unionInfo.Name}.Unknown"
            | _ ->
                line txt $"        let mutable {prefix}{fieldInfo.Name} = {FsharpTypesCmd.defValue true fieldInfo.Type}"

        line txt $"        reader.ReadStartDocument()"

        line txt $"        while reader.State <> BsonReaderState.EndOfDocument do"
        line txt $"            match reader.State with"
        line txt $"            | BsonReaderState.Type -> reader.ReadBsonType() |> ignore"
        line txt $"            | BsonReaderState.Name ->"
        line txt $"                match reader.ReadName() with"

        for fieldInfo in recordInfo.Fields do
            let vName = prefix + fieldInfo.Name

            let suffix =
                match fieldInfo.Type with
                | Optional _ -> "Value"
                | _ -> ""

            linei txt 4 $"| \"{firstCharToUpper fieldInfo.Name}{suffix}\" ->"

            match fieldInfo.Type with
            | Array t
            | List t ->
                linei txt 5 $"reader.ReadStartArray()"
                linei txt 5 $"while reader.ReadBsonType() <> BsonType.EndOfDocument do"
                linei txt 6 $"match {getValue typesCache t} with"
                linei txt 6 $"| ValueSome v -> {vName}.Add(v)"
                linei txt 6 $"| ValueNone -> ()"
                linei txt 5 $"reader.ReadEndArray()"
            | Map t ->
                linei txt 5 $"reader.ReadStartDocument()"
                linei txt 5 $"while reader.ReadBsonType() <> BsonType.EndOfDocument do"
                linei txt 6 $"let propName = reader.ReadName()"
                linei txt 6 $"match {getValue typesCache t} with"
                linei txt 6 $"| ValueSome v -> {vName}.Add(propName, v)"
                linei txt 6 $"| ValueNone -> ()"
                linei txt 5 $"reader.ReadEndDocument()"
            | Types.IsRecord typesCache _
            | Types.IsUnion typesCache _ -> linei txt 5 $"{vName} <- {getValue typesCache fieldInfo.Type}"
            | Optional t ->
                linei txt 5 $"match {getValue typesCache t} with"
                linei txt 5 $"| ValueSome v -> {vName} <- Some v"
                linei txt 5 $"| ValueNone -> ()"
            | t ->
                linei txt 5 $"match {getValue typesCache t} with"
                linei txt 5 $"| ValueSome v -> {vName} <- v"
                linei txt 5 $"| ValueNone -> ()"

        line txt $"                | _ -> reader.SkipValue()"
        line txt "            | _ -> printfn \"Unexpected state: %A\" reader.State"


    and writeObject prefix recordInfo =
        line txt $"        writer.WriteStartDocument()"

        for fieldInfo in recordInfo.Fields do
            let vName = $"{prefix}{fieldInfo.Name}"

            match fieldInfo.Type with
            | Optional t ->
                let inner = "v"
                linei txt 2 $"match {vName} with"
                linei txt 2 $"| Some v ->"
                linei txt 3 $"writer.WriteName(\"{firstCharToUpper fieldInfo.Name}Value\")"
                linei txt 3 $"{setValue typesCache inner t}"
                linei txt 2 $"| None -> ()"
            | _ ->
                let inner = $"{vName}"
                linei txt 2 $"writer.WriteName(\"{firstCharToUpper fieldInfo.Name}\")"
                linei txt 2 $"{setValue typesCache inner fieldInfo.Type}"

        line txt $"        writer.WriteEndDocument()"

    line txt $"namespace {genNamespace}"
    line txt $"open MongoDB.Bson"
    line txt $"open MongoDB.Bson.IO"
    line txt $"open MongoDB.Bson.Serialization"
    line txt $"open MongoDB.Bson.Serialization.Serializers"
    line txt $"open Protokeep"
    let className = $"Convert{solidName module'.Name}"
    line txt $"type {className} () ="

    for item in module'.Items do
        genItem item

    let typeNames =
        module'.Items
        |> Seq.collect (function
            | Record info -> [ info.Name ]
            | Union info -> [ info.Name ]
            | _ -> [])

    for typeName in typeNames do
        line txt $"type {typeName |> firstName}Serializer() ="
        line txt $"    inherit SerializerBase<{typeName |> dottedName}>()"
        line txt $"    override x.Deserialize(ctx: BsonDeserializationContext, args: BsonDeserializationArgs) ="
        line txt $"        {className}.{typeName |> firstName}FromBson(ctx.Reader)"

        line
            txt
            $"    override x.Serialize(ctx: BsonSerializationContext, args: BsonSerializationArgs, value: {typeName |> dottedName}) ="

        line txt $"        {className}.{typeName |> firstName}ToBson(ctx.Writer, value)"

    line txt $"module Serializers ="
    line txt $"    let registerConverters () ="
    line txt $"        BsonDefaults.GuidRepresentationMode <- GuidRepresentationMode.V3"

    for typeName in typeNames do
        line txt $"        BsonSerializer.RegisterSerializer({typeName |> firstName}Serializer())"

    txt.ToString()

let rec getValue (typesCache: Types.TypesCache) =
    function
    | Bool -> $"{helpers}.readBoolean reader"
    | String -> $"{helpers}.readString reader"
    | Int -> $"{helpers}.readInt reader"
    | Long -> $"{helpers}.readLong reader"
    | Float -> $"{helpers}.readFloat reader"
    | Double -> $"{helpers}.readDouble reader"
    | Decimal scale -> $"{helpers}.readMoney(reader, {scale})"
    | Bytes -> $"{helpers}.readBytes reader"
    | Timestamp -> $"{helpers}.readTimestamp reader"
    | Duration -> $"{helpers}.readDuration reader"
    | Guid -> $"{helpers}.readGuid reader"
    | Optional t -> $"{getValue typesCache t}"
    | Types.IsEnum typesCache ei ->
        $"{helpers}.readInt reader |> ValueOption.map (fun v -> LanguagePrimitives.EnumOfValue v)"
    | Complex typeName -> $"Convert{lastNames typeName |> solidName}.{firstName typeName}FromBson(reader)"
    | Array _
    | List _
    | Map _ -> failwith $"Collection in {nameof (getValue)}"

let rec setValue (typesCache: Types.TypesCache) vName type' =
    let rec f vName =
        function
        | Bool -> $"writer.WriteBoolean({vName})"
        | String -> $"writer.WriteString({vName})"
        | Int -> $"writer.WriteInt32({vName})"
        | Long -> $"writer.WriteInt64({vName})"
        | Float -> $"writer.WriteDouble({vName} |> double)"
        | Double -> $"writer.WriteDouble({vName})"
        | Decimal scale -> $"writer.WriteInt32({helpers}.toMoney ({vName}, {scale}))"
        | Bytes -> $"writer.WriteBytes({vName})"
        | Timestamp -> $"writer.WriteDateTime({helpers}.fromDateTime {vName})"
        | Duration -> $"writer.WriteInt32({vName}.TotalMilliseconds |> int)"
        | Guid -> $"{helpers}.writeGuid(writer, {vName})"
        | Optional _ -> failwith "cannot unpack optional field"
        | Array t
        | List t ->
            let inner = "v"
            $"writer.WriteStartArray(); (for v in {vName} do {setValue typesCache inner t}); writer.WriteEndArray()"
        | Map t ->
            let inner = "pair.Value"
            $"writer.WriteStartDocument(); (for pair in {vName} do writer.WriteName(pair.Key); {setValue typesCache inner t}); writer.WriteEndDocument()"
        | Complex typeName ->
            match typesCache.TryFind typeName with
            | Some(Enum _) -> $"writer.WriteInt32({vName} |> int)"
            | _ -> $"Convert{lastNames typeName |> solidName}.{firstName typeName}ToBson(&writer, {vName})"

    f vName type'

let moduleBody =
    """
    open System
    open MongoDB.Bson

    let private unixEpoch = DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)

    let toDateTime (milliseconds: int64) : DateTime =
        unixEpoch.AddMilliseconds(float milliseconds)

    let fromDateTime (dateTime: DateTime) : int64 =
        int64 (dateTime - unixEpoch).TotalMilliseconds

    let readTimestamp (reader: IO.IBsonReader) =
        match reader.CurrentBsonType with
        | BsonType.DateTime -> reader.ReadDateTime() |> toDateTime |> ValueSome
        | bsonType -> ValueNone

    let toMoney (value: decimal, scale: int) : int =
        int (value * decimal (Math.Pow(10., float scale)))

    let fromMoney (value: int, scale: int) : decimal =
        decimal value / decimal (Math.Pow(10., float scale))

    let readInt (reader: IO.IBsonReader) =
        match reader.CurrentBsonType with
        | BsonType.Int32 -> reader.ReadInt32() |> ValueSome
        | BsonType.Int64 -> int (reader.ReadInt64()) |> ValueSome
        | BsonType.Double -> int (reader.ReadDouble()) |> ValueSome
        | BsonType.String -> int (reader.ReadString()) |> ValueSome
        | _ ->
            reader.SkipValue()
            ValueNone

    let readLong (reader: IO.IBsonReader) =
        match reader.CurrentBsonType with
        | BsonType.Int32 -> int64 (reader.ReadInt32()) |> ValueSome
        | BsonType.Int64 -> reader.ReadInt64() |> ValueSome
        | BsonType.Double -> int64 (reader.ReadDouble()) |> ValueSome
        | BsonType.String -> int64 (reader.ReadString()) |> ValueSome
        | _ ->
            reader.SkipValue()
            ValueNone

    let readFloat (reader: IO.IBsonReader) =
        match reader.CurrentBsonType with
        | BsonType.Int32 -> float32 (reader.ReadInt32()) |> ValueSome
        | BsonType.Int64 -> float32 (reader.ReadInt64()) |> ValueSome
        | BsonType.Double -> reader.ReadDouble() |> float32 |> ValueSome
        | BsonType.String -> reader.ReadString() |> float32 |> ValueSome
        | _ ->
            reader.SkipValue()
            ValueNone

    let readDouble (reader: IO.IBsonReader) =
        match reader.CurrentBsonType with
        | BsonType.Int32 -> float (reader.ReadInt32()) |> ValueSome
        | BsonType.Int64 -> float (reader.ReadInt64()) |> ValueSome
        | BsonType.Double -> reader.ReadDouble() |> ValueSome
        | BsonType.String -> reader.ReadString() |> float |> ValueSome
        | _ ->
            reader.SkipValue()
            ValueNone

    let readBoolean (reader: IO.IBsonReader) =
        match reader.CurrentBsonType with
        | BsonType.Boolean -> reader.ReadBoolean() |> ValueSome
        | BsonType.Int32 -> reader.ReadInt32() |> (fun v -> v <> 0) |> ValueSome
        | _ ->
            reader.SkipValue()
            ValueNone

    let readString (reader: IO.IBsonReader) =
        match reader.CurrentBsonType with
        | BsonType.String -> reader.ReadString() |> ValueSome
        | BsonType.Int32 -> reader.ReadInt32() |> string |> ValueSome
        | BsonType.Int64 -> reader.ReadInt64() |> string |> ValueSome
        | BsonType.Double -> reader.ReadDouble() |> string |> ValueSome
        | _ ->
            reader.SkipValue()
            ValueNone

    let readBytes (reader: IO.IBsonReader) =
        match reader.CurrentBsonType with
        | BsonType.Binary -> reader.ReadBinaryData().Bytes |> ValueSome
        | _ ->
            reader.SkipValue()
            ValueNone

    let readMoney (reader: IO.IBsonReader, scale: int) =
        readInt reader |> ValueOption.map (fun v -> fromMoney (v, scale))

    let readDuration (reader: IO.IBsonReader) =
        readInt reader |> ValueOption.map TimeSpan.FromMilliseconds

    let readGuid (reader: IO.IBsonReader) =
        match reader.CurrentBsonType with
        | BsonType.Binary -> reader.ReadBinaryData().ToGuid() |> ValueSome
        | _ ->
            reader.SkipValue()
            ValueNone

    let writeGuid (writer: IO.IBsonWriter, value: Guid) =
        writer.WriteBinaryData(
            BsonBinaryData(
                GuidConverter.ToBytes(value, GuidRepresentation.Standard),
                GuidConverter.GetSubType(GuidRepresentation.Standard)
            )
        )
"""
