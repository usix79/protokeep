namespace Protokeep.FsharpJsonConverters
open System.Text.Json
open Protokeep.FsharpJsonConvertersHelpers
type ConvertDomain () =
    static member DefaultTrafficLight =
        lazy Domain.TrafficLight.Unknown
    static member TrafficLightFromString = function
        | "TrafficLightRed" -> Domain.TrafficLight.Red
        | "TrafficLightYellow" -> Domain.TrafficLight.Yellow
        | "TrafficLightGreen" -> Domain.TrafficLight.Green
        | _ -> Domain.TrafficLight.Unknown
    static member TrafficLightToString = function
        | Domain.TrafficLight.Red -> "TrafficLightRed"
        | Domain.TrafficLight.Yellow -> "TrafficLightYellow"
        | Domain.TrafficLight.Green -> "TrafficLightGreen"
        | _ -> "Unknown"
    static member LightStatusFromJson (reader: byref<Utf8JsonReader>): Domain.LightStatus =
        let mutable y = Domain.LightStatus.Unknown
        if reader.TokenType = JsonTokenType.StartObject || reader.Read() && reader.TokenType = JsonTokenType.StartObject then
            while reader.Read() && reader.TokenType <> JsonTokenType.EndObject do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Normal")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.True
                    then y <- Domain.LightStatus.Normal
                    else reader.Skip()
                else if (reader.ValueTextEquals("Warning")) then
                    y <- ConvertDomain.LightStatusCaseWarningFromJson(&reader)
                else if (reader.ValueTextEquals("OutOfOrder")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.String
                    then y <- reader.GetDateTime() |> Domain.LightStatus.OutOfOrder
                    else reader.Skip()
                else reader.Skip()
        y
    static member LightStatusToJson (writer:inref<Utf8JsonWriter>, x: Domain.LightStatus) =
        writer.WriteStartObject()
        match x with
        | Domain.LightStatus.Normal ->
            writer.WritePropertyName("Normal")
            writer.WriteBooleanValue(true)
        | Domain.LightStatus.Warning (errorsCount,level) ->
            writer.WritePropertyName("Warning")
            ConvertDomain.LightStatusCaseWarningToJson(&writer,errorsCount,level)
        | Domain.LightStatus.OutOfOrder (since) ->
            writer.WritePropertyName("OutOfOrder")
            writer.WriteStringValue(since |> fromDateTime)
        | _ ->
            writer.WritePropertyName("Unknown")
            writer.WriteBooleanValue(true)
        writer.WriteEndObject()
    static member LightStatusCaseWarningFromJson (reader: byref<Utf8JsonReader>) =
        let mutable errorsCount = 0
        let mutable level = 0
        if reader.TokenType = JsonTokenType.StartObject || reader.Read() && reader.TokenType = JsonTokenType.StartObject then
            while (reader.Read() && reader.TokenType <> JsonTokenType.EndObject) do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("ErrorsCount")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.Number
                    then errorsCount <- reader.GetInt32()
                    else reader.Skip()
                else if (reader.ValueTextEquals("Level")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.Number
                    then level <- reader.GetInt32()
                    else reader.Skip()
                else reader.Skip()
        Domain.LightStatus.Warning (errorsCount,level)
    static member LightStatusCaseWarningToJson (writer: inref<Utf8JsonWriter>,errorsCount,level) =
        writer.WriteStartObject()
        writer.WritePropertyName("ErrorsCount")
        writer.WriteNumberValue(errorsCount)
        writer.WritePropertyName("Level")
        writer.WriteNumberValue(level)
        writer.WriteEndObject()
    static member DefaultCrossroad: Lazy<Domain.Crossroad> =
        lazy {
            Id = 0
            LongId = 0L
            AltId = System.Guid.Empty
            Street1 = ""
            Street2 = ""
            IsMonitored = false
            Xpos = 0.f
            Ypos = 0.
            Ratio = 0m
            LastChecked = System.DateTime.MinValue
            ServiceInterval = System.TimeSpan.Zero
            CurrentLight = ConvertDomain.DefaultTrafficLight.Value
            Nickname = None
            Img = Array.empty
            Notes = Array.empty
            Props = Map.empty
        }
    static member CrossroadFromJson (reader: byref<Utf8JsonReader>): Domain.Crossroad =
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
        let mutable vCurrentLight = ConvertDomain.DefaultTrafficLight.Value
        let mutable vNickname = None
        let mutable vImg = Array.empty
        let mutable vNotes = ResizeArray()
        let mutable vProps = ResizeArray()
        if reader.TokenType = JsonTokenType.StartObject || reader.Read() && reader.TokenType = JsonTokenType.StartObject then
            while (reader.Read() && reader.TokenType <> JsonTokenType.EndObject) do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Id")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.Number
                    then vId <- reader.GetInt32()
                    else reader.Skip()
                else if (reader.ValueTextEquals("LongId")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.Number
                    then vLongId <- reader.GetInt64()
                    else reader.Skip()
                else if (reader.ValueTextEquals("AltId")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.String
                    then vAltId <- System.Guid(reader.GetBytesFromBase64())
                    else reader.Skip()
                else if (reader.ValueTextEquals("Street1")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.String
                    then vStreet1 <- reader.GetString()
                    else reader.Skip()
                else if (reader.ValueTextEquals("Street2")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.String
                    then vStreet2 <- reader.GetString()
                    else reader.Skip()
                else if (reader.ValueTextEquals("IsMonitored")) then
                    if reader.Read() && (reader.TokenType = JsonTokenType.True || reader.TokenType = JsonTokenType.False)
                    then vIsMonitored <- reader.GetBoolean()
                    else reader.Skip()
                else if (reader.ValueTextEquals("Xpos")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.Number
                    then vXpos <- reader.GetSingle()
                    else reader.Skip()
                else if (reader.ValueTextEquals("Ypos")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.Number
                    then vYpos <- reader.GetDouble()
                    else reader.Skip()
                else if (reader.ValueTextEquals("Ratio")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.Number
                    then vRatio <- decimal(float(reader.GetInt64()) / 100.)
                    else reader.Skip()
                else if (reader.ValueTextEquals("LastChecked")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.String
                    then vLastChecked <- reader.GetDateTime()
                    else reader.Skip()
                else if (reader.ValueTextEquals("ServiceInterval")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.String
                    then vServiceInterval <- reader.GetString() |> toTimeSpan
                    else reader.Skip()
                else if (reader.ValueTextEquals("CurrentLight")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.String
                    then vCurrentLight <- reader.GetString() |> ConvertDomain.TrafficLightFromString
                    else reader.Skip()
                else if (reader.ValueTextEquals("NicknameValue")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.String
                    then vNickname <- reader.GetString() |> Some
                    else reader.Skip()
                else if (reader.ValueTextEquals("Img")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.String
                    then vImg <- reader.GetBytesFromBase64()
                    else reader.Skip()
                else if (reader.ValueTextEquals("Notes")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.StartArray then
                        while reader.Read() && reader.TokenType <> JsonTokenType.EndArray do
                            if reader.TokenType = JsonTokenType.String then
                                vNotes.Add(reader.GetString())
                            else reader.Skip()
                    else reader.Skip()
                else if (reader.ValueTextEquals("Props")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.StartObject then
                        while reader.Read() && reader.TokenType <> JsonTokenType.EndObject do
                            let propName = reader.GetString()
                            if reader.Read() && reader.TokenType = JsonTokenType.String then
                                vProps.Add((propName, reader.GetString()))
                            else reader.Skip()
                    else reader.Skip()
                else reader.Skip()
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
    static member CrossroadToJson (writer: inref<Utf8JsonWriter>, x: Domain.Crossroad) =
        writer.WriteStartObject()
        writer.WritePropertyName("Id")
        writer.WriteNumberValue(x.Id)
        writer.WritePropertyName("LongId")
        writer.WriteNumberValue(x.LongId)
        writer.WritePropertyName("AltId")
        writer.WriteBase64StringValue(System.ReadOnlySpan(x.AltId.ToByteArray()))
        writer.WritePropertyName("Street1")
        writer.WriteStringValue(x.Street1)
        writer.WritePropertyName("Street2")
        writer.WriteStringValue(x.Street2)
        writer.WritePropertyName("IsMonitored")
        writer.WriteBooleanValue(x.IsMonitored)
        writer.WritePropertyName("Xpos")
        writer.WriteNumberValue(x.Xpos)
        writer.WritePropertyName("Ypos")
        writer.WriteNumberValue(x.Ypos)
        writer.WritePropertyName("Ratio")
        writer.WriteNumberValue(x.Ratio * 100m |> System.Decimal.Truncate)
        writer.WritePropertyName("LastChecked")
        writer.WriteStringValue(x.LastChecked |> fromDateTime)
        writer.WritePropertyName("ServiceInterval")
        writer.WriteStringValue(x.ServiceInterval |> fromTimeSpan)
        writer.WritePropertyName("CurrentLight")
        writer.WriteStringValue(x.CurrentLight |> ConvertDomain.TrafficLightToString)
        match x.Nickname with
        | Some v ->
            writer.WritePropertyName("NicknameValue")
            writer.WriteStringValue(v)
        | None -> ()
        writer.WritePropertyName("Img")
        writer.WriteBase64StringValue(System.ReadOnlySpan(x.Img))
        writer.WritePropertyName("Notes")
        writer.WriteStartArray(); (for v in x.Notes do writer.WriteStringValue(v)); writer.WriteEndArray()
        writer.WritePropertyName("Props")
        writer.WriteStartObject(); (for pair in x.Props do writer.WritePropertyName(pair.Key); writer.WriteStringValue(pair.Value)); writer.WriteEndObject()
        writer.WriteEndObject()
