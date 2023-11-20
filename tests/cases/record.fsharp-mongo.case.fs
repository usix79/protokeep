namespace Test.Converters

open MongoDB.Bson
open MongoDB.Bson.IO
open MongoDB.Bson.Serialization
open MongoDB.Bson.Serialization.Serializers
open Protokeep

type ConvertTestDomain() =
    static member CrossroadToBson(writer: IBsonWriter, x: Test.Domain.Crossroad, ?asEntity: bool) =
        writer.WriteStartDocument()
        if asEntity.IsSome then
            FsharpMongoHelpers.writeId (writer, x)
        writer.WriteName("Id")
        writer.WriteInt32(x.Id)
        writer.WriteName("AltId")
        FsharpMongoHelpers.writeGuid(writer, x.AltId)
        writer.WriteName("Address")
        writer.WriteString(x.Address)
        match x.Corner with
        | ValueSome v ->
            writer.WriteName("CornerValue")
            writer.WriteString(v)
        | ValueNone -> ()
        writer.WriteName("IsMonitored")
        writer.WriteBoolean(x.IsMonitored)
        writer.WriteName("Patch")
        writer.WriteInt32(int x.Patch)
        writer.WriteName("Model")
        writer.WriteInt32(int x.Model)
        writer.WriteName("Serial")
        writer.WriteInt32(x.Serial)
        writer.WriteName("Mask")
        writer.WriteInt64(x.Mask)
        writer.WriteName("Cost")
        writer.WriteInt32(FsharpMongoHelpers.toMoney (x.Cost, 2))
        writer.WriteName("Xpos")
        writer.WriteDouble(double x.Xpos)
        writer.WriteName("Ypos")
        writer.WriteDouble(x.Ypos)
        writer.WriteName("LastChecked")
        writer.WriteDateTime(FsharpMongoHelpers.fromDateTime x.LastChecked)
        writer.WriteName("ServiceInterval")
        writer.WriteInt32(x.ServiceInterval.TotalMilliseconds |> int)
        writer.WriteName("Intervals")
        writer.WriteStartArray()
        for v in x.Intervals do
            writer.WriteInt32(v)
        writer.WriteEndArray()
        writer.WriteName("Notes")
        writer.WriteStartArray()
        for v in x.Notes do
            writer.WriteString(v)
        writer.WriteEndArray()
        writer.WriteName("Tags")
        writer.WriteStartArray()
        for v in x.Tags do
            writer.WriteString(v)
        writer.WriteEndArray()
        writer.WriteName("Metrics")
        writer.WriteStartArray()
        for pair in x.Metrics do
            writer.WriteStartDocument()
            writer.WriteName("Key")
            writer.WriteString(pair.Key)
            writer.WriteName("Value")
            writer.WriteInt32(pair.Value)
            writer.WriteEndDocument()
        writer.WriteEndArray()
        match x.Next with
        | ValueSome v ->
            writer.WriteName("NextValue")
            ConvertTestDomain.CrossroadToBson(writer, v)
        | ValueNone -> ()
        writer.WriteName("Img")
        writer.WriteBytes(x.Img)
        writer.WriteName("Version")
        writer.WriteInt32(x.Version)
        writer.WriteEndDocument()

    static member CrossroadFromBson(reader: IBsonReader): Test.Domain.Crossroad =
        let mutable vId = 0
        let mutable vAltId = System.Guid.Empty
        let mutable vAddress = ""
        let mutable vCorner = ValueNone
        let mutable vIsMonitored = false
        let mutable vPatch = 0y
        let mutable vModel = 0s
        let mutable vSerial = 0
        let mutable vMask = 0L
        let mutable vCost = 0m
        let mutable vXpos = 0.f
        let mutable vYpos = 0.
        let mutable vLastChecked = System.DateTime.MinValue
        let mutable vServiceInterval = System.TimeSpan.Zero
        let mutable vIntervals = ResizeArray()
        let mutable vNotes = ResizeArray()
        let mutable vTags = ResizeArray()
        let mutable vMetrics = ResizeArray()
        let mutable vNext = ValueNone
        let mutable vImg = Array.empty
        let mutable vVersion = 0
        reader.ReadStartDocument()
        while reader.State <> BsonReaderState.EndOfDocument do
            match reader.State with
            | BsonReaderState.Type -> reader.ReadBsonType() |> ignore
            | BsonReaderState.Name ->
                match reader.ReadName() with
                | "Id" ->
                    match FsharpMongoHelpers.readInt32 reader with
                    | ValueSome v -> vId <- v
                    | ValueNone -> ()
                | "AltId" ->
                    match FsharpMongoHelpers.readGuid reader with
                    | ValueSome v -> vAltId <- v
                    | ValueNone -> ()
                | "Address" ->
                    match FsharpMongoHelpers.readString reader with
                    | ValueSome v -> vAddress <- v
                    | ValueNone -> ()
                | "CornerValue" ->
                    match FsharpMongoHelpers.readString reader |> ValueOption.map ValueSome with
                    | ValueSome v -> vCorner <- v
                    | ValueNone -> ()
                | "IsMonitored" ->
                    match FsharpMongoHelpers.readBoolean reader with
                    | ValueSome v -> vIsMonitored <- v
                    | ValueNone -> ()
                | "Patch" ->
                    match FsharpMongoHelpers.readInt8 reader with
                    | ValueSome v -> vPatch <- v
                    | ValueNone -> ()
                | "Model" ->
                    match FsharpMongoHelpers.readInt16 reader with
                    | ValueSome v -> vModel <- v
                    | ValueNone -> ()
                | "Serial" ->
                    match FsharpMongoHelpers.readInt32 reader with
                    | ValueSome v -> vSerial <- v
                    | ValueNone -> ()
                | "Mask" ->
                    match FsharpMongoHelpers.readInt64 reader with
                    | ValueSome v -> vMask <- v
                    | ValueNone -> ()
                | "Cost" ->
                    match FsharpMongoHelpers.readMoney(reader, 2) with
                    | ValueSome v -> vCost <- v
                    | ValueNone -> ()
                | "Xpos" ->
                    match FsharpMongoHelpers.readFloat32 reader with
                    | ValueSome v -> vXpos <- v
                    | ValueNone -> ()
                | "Ypos" ->
                    match FsharpMongoHelpers.readFloat64 reader with
                    | ValueSome v -> vYpos <- v
                    | ValueNone -> ()
                | "LastChecked" ->
                    match FsharpMongoHelpers.readTimestamp reader with
                    | ValueSome v -> vLastChecked <- v
                    | ValueNone -> ()
                | "ServiceInterval" ->
                    match FsharpMongoHelpers.readDuration reader with
                    | ValueSome v -> vServiceInterval <- v
                    | ValueNone -> ()
                | "Intervals" ->
                    reader.ReadStartArray()
                    while reader.ReadBsonType() <> BsonType.EndOfDocument do
                        match FsharpMongoHelpers.readInt32 reader with
                        | ValueSome v -> vIntervals.Add(v)
                        | ValueNone -> ()
                    reader.ReadEndArray()
                | "Notes" ->
                    reader.ReadStartArray()
                    while reader.ReadBsonType() <> BsonType.EndOfDocument do
                        match FsharpMongoHelpers.readString reader with
                        | ValueSome v -> vNotes.Add(v)
                        | ValueNone -> ()
                    reader.ReadEndArray()
                | "Tags" ->
                    reader.ReadStartArray()
                    while reader.ReadBsonType() <> BsonType.EndOfDocument do
                        match FsharpMongoHelpers.readString reader with
                        | ValueSome v -> vTags.Add(v)
                        | ValueNone -> ()
                    reader.ReadEndArray()
                | "Metrics" ->
                    reader.ReadStartArray()
                    while reader.ReadBsonType() <> BsonType.EndOfDocument do
                        reader.ReadStartDocument()
                        let mutable key = ""
                        let mutable value = 0
                        while reader.ReadBsonType() <> BsonType.EndOfDocument do
                            match reader.ReadName() with
                            | "Key" ->
                                match FsharpMongoHelpers.readString reader with
                                | ValueSome v -> key <- v
                                | ValueNone -> ()
                            | "Value" ->
                                match FsharpMongoHelpers.readInt32 reader with
                                | ValueSome v -> value <- v
                                | ValueNone -> ()
                            | _ -> reader.SkipValue()
                        reader.ReadEndDocument()
                        vMetrics.Add(key, value)
                    reader.ReadEndArray()
                | "NextValue" ->
                    match ConvertTestDomain.CrossroadFromBson(reader) |> ValueSome |> ValueOption.map ValueSome with
                    | ValueSome v -> vNext <- v
                    | ValueNone -> ()
                | "Img" ->
                    match FsharpMongoHelpers.readBinary reader with
                    | ValueSome v -> vImg <- v
                    | ValueNone -> ()
                | "Version" ->
                    match FsharpMongoHelpers.readInt32 reader with
                    | ValueSome v -> vVersion <- v
                    | ValueNone -> ()
                | _ -> reader.SkipValue()
            | _ -> printfn "Unexpected state: %A" reader.State
        reader.ReadEndDocument()
        {
            Id = vId
            AltId = vAltId
            Address = vAddress
            Corner = vCorner
            IsMonitored = vIsMonitored
            Patch = vPatch
            Model = vModel
            Serial = vSerial
            Mask = vMask
            Cost = vCost
            Xpos = vXpos
            Ypos = vYpos
            LastChecked = vLastChecked
            ServiceInterval = vServiceInterval
            Intervals = vIntervals |> List.ofSeq
            Notes = vNotes |> Array.ofSeq
            Tags = vTags |> Set.ofSeq
            Metrics = vMetrics |> Map.ofSeq
            Next = vNext
            Img = vImg
            Version = vVersion
        }


type CrossroadSerializer() =
    inherit SerializerBase<Test.Domain.Crossroad>()
    override x.Deserialize(ctx: BsonDeserializationContext, args: BsonDeserializationArgs) =
        ConvertTestDomain.CrossroadFromBson(ctx.Reader)

    override x.Serialize(ctx: BsonSerializationContext, args: BsonSerializationArgs, value: Test.Domain.Crossroad) =
        ConvertTestDomain.CrossroadToBson(ctx.Writer, value, true)

type ConvertTestDomain with
    static member RegisterSerializers() =
        BsonDefaults.GuidRepresentationMode <- GuidRepresentationMode.V3
        BsonSerializer.RegisterSerializer(GuidSerializer(GuidRepresentation.Standard))
        BsonSerializer.RegisterSerializer(CrossroadSerializer())
