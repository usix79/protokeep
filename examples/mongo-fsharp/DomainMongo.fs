namespace Domain

open MongoDB.Bson
open MongoDB.Bson.IO
open MongoDB.Bson.Serialization
open MongoDB.Bson.Serialization.Serializers
open Protokeep

type ConvertDomain() =
    static memberTrafficLightFromInt = function
        | Domain.TrafficLight.Red -> Domain.TrafficLight.Red
        | Domain.TrafficLight.Yellow -> Domain.TrafficLight.Yellow
        | Domain.TrafficLight.Green -> Domain.TrafficLight.Green
        | _ -> Domain.TrafficLight.Unknown

    static member LightStatusToBson(writer: IBsonWriter, x: Domain.LightStatus) =
        writer.WriteStartDocument()
        match x with
        | Domain.LightStatus.Normal ->
            writer.WriteName("Normal")
            writer.WriteBoolean(true)
        | Domain.LightStatus.Warning (errorsCount,level) ->
            writer.WriteName("Warning")
            ConvertDomain.LightStatusCaseWarningToBson(writer,errorsCount,level)
        | Domain.LightStatus.OutOfOrder (since) ->
            writer.WriteName("OutOfOrder")
            writer.WriteDateTime(FsharpMongoHelpers.fromDateTime since)
        | _ ->
            writer.WriteName("Unknown")
            writer.WriteBoolean(true)
        writer.WriteEndDocument()
    static member LightStatusCaseWarningToBson (writer: IBsonWriter,errorsCount,level) =
        writer.WriteStartDocument()
        writer.WriteName("ErrorsCount")
        writer.WriteInt32(errorsCount)
        writer.WriteName("Level")
        writer.WriteInt32(level)
        writer.WriteEndDocument()
    static member LightStatusFromBson (reader: IBsonReader): Domain.LightStatus =
        let mutable y = Domain.LightStatus.Unknown
        reader.ReadStartDocument()
        match reader.ReadName() with
                | "Normal" ->
                    match FsharpMongoHelpers.readBoolean reader with
                    | ValueSome true -> y <- Domain.LightStatus.Normal
                    | _ -> ()
                | "Warning" ->
                    y <- ConvertDomain.LightStatusCaseWarningFromBson(reader)
                | "OutOfOrder" ->
                    match FsharpMongoHelpers.readTimestamp reader with
                    | ValueSome v -> y <- v |> Domain.LightStatus.OutOfOrder
                    | _ -> ()
                | _ -> ()
        reader.ReadEndDocument()
        y
    static member LightStatusCaseWarningFromBson (reader: IBsonReader) =
        let mutable errorsCount = 0
        let mutable level = 0
        reader.ReadStartDocument()
        while reader.State <> BsonReaderState.EndOfDocument do
            match reader.State with
            | BsonReaderState.Type -> reader.ReadBsonType() |> ignore
            | BsonReaderState.Name ->
                match reader.ReadName() with
                | "ErrorsCount" ->
                    match FsharpMongoHelpers.readInt reader with
                    | ValueSome v -> errorsCount <- v
                    | ValueNone -> ()
                | "Level" ->
                    match FsharpMongoHelpers.readInt reader with
                    | ValueSome v -> level <- v
                    | ValueNone -> ()
                | _ -> reader.SkipValue()
            | _ -> printfn "Unexpected state: %A" reader.State
        reader.ReadEndDocument()
        Domain.LightStatus.Warning (errorsCount,level)
    static member CrossroadToBson(writer: IBsonWriter, x: Domain.Crossroad, ?asEntity: bool) =
        writer.WriteStartDocument()
        if asEntity.IsSome then
            FsharpMongoHelpers.writeId (writer, x)
        writer.WriteName("Id")
        writer.WriteInt32(x.Id)
        writer.WriteName("LongId")
        writer.WriteInt64(x.LongId)
        writer.WriteName("AltId")
        FsharpMongoHelpers.writeGuid(writer, x.AltId)
        writer.WriteName("Street1")
        writer.WriteString(x.Street1)
        writer.WriteName("Street2")
        writer.WriteString(x.Street2)
        writer.WriteName("IsMonitored")
        writer.WriteBoolean(x.IsMonitored)
        writer.WriteName("Xpos")
        writer.WriteDouble(x.Xpos |> double)
        writer.WriteName("Ypos")
        writer.WriteDouble(x.Ypos)
        writer.WriteName("Ratio")
        writer.WriteInt32(FsharpMongoHelpers.toMoney (x.Ratio, 2))
        writer.WriteName("LastChecked")
        writer.WriteDateTime(FsharpMongoHelpers.fromDateTime x.LastChecked)
        writer.WriteName("ServiceInterval")
        writer.WriteInt32(x.ServiceInterval.TotalMilliseconds |> int)
        writer.WriteName("CurrentLight")
        writer.WriteInt32(x.CurrentLight |> int)
        match x.Nickname with
        | ValueSome v ->
            writer.WriteName("NicknameValue")
            writer.WriteString(v)
        | ValueNone -> ()
        writer.WriteName("Img")
        writer.WriteBytes(x.Img)
        writer.WriteName("Notes")
        writer.WriteStartArray(); (for v in x.Notes do writer.WriteString(v)); writer.WriteEndArray()
        writer.WriteName("Props")
        writer.WriteStartDocument(); (for pair in x.Props do writer.WriteName(pair.Key); writer.WriteString(pair.Value)); writer.WriteEndDocument()
        writer.WriteEndDocument()

    static member CrossroadFromBson(reader: IBsonReader): Domain.Crossroad =
        let mutable vId = 0
        let mutable vLongId = 0L
        let mutable vAltId = System.Guid.Empty
        let mutable vStreet1 = ""
        let mutable vStreet2 = ""
        let mutable vIsMonitored = false
        let mutable vXpos = 0.f
        let mutable vYpos = 0.
        let mutable vRatio = 0m
        let mutable vLastChecked = System.DateTime.MinValue
        let mutable vServiceInterval = System.TimeSpan.Zero
        let mutable vCurrentLight = Domain.TrafficLight.Unknown
        let mutable vNickname = ValueNone
        let mutable vImg = Array.empty
        let mutable vNotes = ResizeArray()
        let mutable vProps = ResizeArray()
        reader.ReadStartDocument()
        while reader.State <> BsonReaderState.EndOfDocument do
            match reader.State with
            | BsonReaderState.Type -> reader.ReadBsonType() |> ignore
            | BsonReaderState.Name ->
                match reader.ReadName() with
                | "Id" ->
                    match FsharpMongoHelpers.readInt reader with
                    | ValueSome v -> vId <- v
                    | ValueNone -> ()
                | "LongId" ->
                    match FsharpMongoHelpers.readLong reader with
                    | ValueSome v -> vLongId <- v
                    | ValueNone -> ()
                | "AltId" ->
                    match FsharpMongoHelpers.readGuid reader with
                    | ValueSome v -> vAltId <- v
                    | ValueNone -> ()
                | "Street1" ->
                    match FsharpMongoHelpers.readString reader with
                    | ValueSome v -> vStreet1 <- v
                    | ValueNone -> ()
                | "Street2" ->
                    match FsharpMongoHelpers.readString reader with
                    | ValueSome v -> vStreet2 <- v
                    | ValueNone -> ()
                | "IsMonitored" ->
                    match FsharpMongoHelpers.readBoolean reader with
                    | ValueSome v -> vIsMonitored <- v
                    | ValueNone -> ()
                | "Xpos" ->
                    match FsharpMongoHelpers.readFloat reader with
                    | ValueSome v -> vXpos <- v
                    | ValueNone -> ()
                | "Ypos" ->
                    match FsharpMongoHelpers.readDouble reader with
                    | ValueSome v -> vYpos <- v
                    | ValueNone -> ()
                | "Ratio" ->
                    match FsharpMongoHelpers.readMoney(reader, 2) with
                    | ValueSome v -> vRatio <- v
                    | ValueNone -> ()
                | "LastChecked" ->
                    match FsharpMongoHelpers.readTimestamp reader with
                    | ValueSome v -> vLastChecked <- v
                    | ValueNone -> ()
                | "ServiceInterval" ->
                    match FsharpMongoHelpers.readDuration reader with
                    | ValueSome v -> vServiceInterval <- v
                    | ValueNone -> ()
                | "CurrentLight" ->
                    match FsharpMongoHelpers.readInt reader |> ValueOption.map (fun v -> LanguagePrimitives.EnumOfValue v) with
                    | ValueSome v -> vCurrentLight <- v
                    | ValueNone -> ()
                | "NicknameValue" ->
                    match FsharpMongoHelpers.readString reader |> ValueOption.map ValueSome with
                    | ValueSome v -> vNickname <- v
                    | ValueNone -> ()
                | "Img" ->
                    match FsharpMongoHelpers.readBytes reader with
                    | ValueSome v -> vImg <- v
                    | ValueNone -> ()
                | "Notes" ->
                    reader.ReadStartArray()
                    while reader.ReadBsonType() <> BsonType.EndOfDocument do
                        match FsharpMongoHelpers.readString reader with
                        | ValueSome v -> vNotes.Add(v)
                        | ValueNone -> ()
                    reader.ReadEndArray()
                | "Props" ->
                    reader.ReadStartDocument()
                    while reader.ReadBsonType() <> BsonType.EndOfDocument do
                        let propName = reader.ReadName()
                        match FsharpMongoHelpers.readString reader with
                        | ValueSome v -> vProps.Add(propName, v)
                        | ValueNone -> ()
                    reader.ReadEndDocument()
                | _ -> reader.SkipValue()
            | _ -> printfn "Unexpected state: %A" reader.State
        reader.ReadEndDocument()
        {
            Id = vId
            LongId = vLongId
            AltId = vAltId
            Street1 = vStreet1
            Street2 = vStreet2
            IsMonitored = vIsMonitored
            Xpos = vXpos
            Ypos = vYpos
            Ratio = vRatio
            LastChecked = vLastChecked
            ServiceInterval = vServiceInterval
            CurrentLight = vCurrentLight
            Nickname = vNickname
            Img = vImg
            Notes = vNotes |> Array.ofSeq
            Props = vProps |> Map.ofSeq
        }


type CrossroadSerializer() =
    inherit SerializerBase<Domain.Crossroad>()
    override x.Deserialize(ctx: BsonDeserializationContext, args: BsonDeserializationArgs) =
        ConvertDomain.CrossroadFromBson(ctx.Reader)

    override x.Serialize(ctx: BsonSerializationContext, args: BsonSerializationArgs, value: Domain.Crossroad) =
        ConvertDomain.CrossroadToBson(ctx.Writer, value, true)

type ConvertDomain with
    static member RegisterSerializers() =
        BsonDefaults.GuidRepresentationMode <- GuidRepresentationMode.V3
        BsonSerializer.RegisterSerializer(CrossroadSerializer())
