namespace Protokeep.FsharpJson

open System.Text.Json
open Protokeep

type ConvertDomain() =

    static member TrafficLightFromString =
        function
        | "TrafficLightRed" -> Domain.TrafficLight.Red
        | "TrafficLightYellow" -> Domain.TrafficLight.Yellow
        | "TrafficLightGreen" -> Domain.TrafficLight.Green
        | _ -> Domain.TrafficLight.Unknown

    static member TrafficLightToString =
        function
        | Domain.TrafficLight.Red -> "TrafficLightRed"
        | Domain.TrafficLight.Yellow -> "TrafficLightYellow"
        | Domain.TrafficLight.Green -> "TrafficLightGreen"
        | _ -> "Unknown"

    static member LightStatusFromJson(reader: byref<Utf8JsonReader>) : Domain.LightStatus =
        let mutable y = Domain.LightStatus.Unknown

        if FsharpJsonHelpers.moveToStartObject (&reader) then
            while FsharpJsonHelpers.moveToEndObject (&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then
                    ()
                else if (reader.ValueTextEquals("Normal")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.True then
                        y <- Domain.LightStatus.Normal
                    else
                        reader.Skip()
                else if (reader.ValueTextEquals("Warning")) then
                    match FsharpJsonHelpers.readInt (&reader) with
                    | ValueSome v -> y <- v |> Domain.LightStatus.Warning
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("OutOfOrder")) then
                    match FsharpJsonHelpers.readTimestamp (&reader) with
                    | ValueSome v -> y <- v |> Domain.LightStatus.OutOfOrder
                    | ValueNone -> ()
                else
                    reader.Skip()

        y

    static member LightStatusToJson(writer: inref<Utf8JsonWriter>, x: Domain.LightStatus) =
        writer.WriteStartObject()

        match x with
        | Domain.LightStatus.Normal ->
            writer.WritePropertyName("Normal")
            writer.WriteBooleanValue(true)
        | Domain.LightStatus.Warning(errorsCount) ->
            writer.WritePropertyName("Warning")
            writer.WriteNumberValue(errorsCount)
        | Domain.LightStatus.OutOfOrder(since) ->
            writer.WritePropertyName("OutOfOrder")
            FsharpJsonHelpers.writeTimestamp (&writer, since)
        | _ ->
            writer.WritePropertyName("Unknown")
            writer.WriteBooleanValue(true)

        writer.WriteEndObject()

    static member CrossroadFromJson(reader: byref<Utf8JsonReader>) : Domain.Crossroad =
        let mutable vId = 0
        let mutable vStreet1 = ""
        let mutable vStreet2 = ""
        let mutable vLight = Domain.TrafficLight.Unknown
        let mutable vLightStatus = Domain.LightStatus.Unknown
        let mutable vHistory = ResizeArray()
        let mutable vLirycs = ResizeArray()

        if FsharpJsonHelpers.moveToStartObject (&reader) then
            while FsharpJsonHelpers.moveToEndObject (&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then
                    ()
                else if (reader.ValueTextEquals("Id")) then
                    match FsharpJsonHelpers.readInt (&reader) with
                    | ValueSome v -> vId <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Street1")) then
                    match FsharpJsonHelpers.readString (&reader) with
                    | ValueSome v -> vStreet1 <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Street2")) then
                    match FsharpJsonHelpers.readString (&reader) with
                    | ValueSome v -> vStreet2 <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Light")) then
                    match
                        FsharpJsonHelpers.readString (&reader)
                        |> ValueOption.map ConvertDomain.TrafficLightFromString
                    with
                    | ValueSome v -> vLight <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("LightStatus")) then
                    match ConvertDomain.LightStatusFromJson(&reader) |> ValueSome with
                    | ValueSome v -> vLightStatus <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("History")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.StartArray then
                        while reader.Read() && reader.TokenType <> JsonTokenType.EndArray do
                            match ConvertDomain.LightStatusFromJson(&reader) |> ValueSome with
                            | ValueSome v -> vHistory.Add(v)
                            | ValueNone -> ()
                    else
                        reader.Skip()
                else if (reader.ValueTextEquals("Lirycs")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.StartArray then
                        while reader.Read() && reader.TokenType <> JsonTokenType.EndArray do
                            match FsharpJsonHelpers.readString (&reader) with
                            | ValueSome v -> vLirycs.Add(v)
                            | ValueNone -> ()
                    else
                        reader.Skip()
                else
                    reader.Skip()

        { Id = vId
          Street1 = vStreet1
          Street2 = vStreet2
          Light = vLight
          LightStatus = vLightStatus
          History = vHistory |> List.ofSeq
          Lirycs = vLirycs |> List.ofSeq }

    static member CrossroadToJson(writer: inref<Utf8JsonWriter>, x: Domain.Crossroad) =
        writer.WriteStartObject()
        writer.WritePropertyName("Id")
        writer.WriteNumberValue(x.Id)
        writer.WritePropertyName("Street1")
        writer.WriteStringValue(x.Street1)
        writer.WritePropertyName("Street2")
        writer.WriteStringValue(x.Street2)
        writer.WritePropertyName("Light")
        writer.WriteStringValue(x.Light |> ConvertDomain.TrafficLightToString)
        writer.WritePropertyName("LightStatus")
        ConvertDomain.LightStatusToJson(&writer, x.LightStatus)
        writer.WritePropertyName("History")
        writer.WriteStartArray()

        (for v in x.History do
            ConvertDomain.LightStatusToJson(&writer, v))

        writer.WriteEndArray()
        writer.WritePropertyName("Lirycs")
        writer.WriteStartArray()

        (for v in x.Lirycs do
            writer.WriteStringValue(v))

        writer.WriteEndArray()
        writer.WriteEndObject()

    static member Crossroad2FromJson(reader: byref<Utf8JsonReader>) : Domain.Crossroad2 =
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

        if FsharpJsonHelpers.moveToStartObject (&reader) then
            while FsharpJsonHelpers.moveToEndObject (&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then
                    ()
                else if (reader.ValueTextEquals("Id")) then
                    match FsharpJsonHelpers.readInt (&reader) with
                    | ValueSome v -> vId <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("LongId")) then
                    match FsharpJsonHelpers.readLong (&reader) with
                    | ValueSome v -> vLongId <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("AltId")) then
                    match FsharpJsonHelpers.readGuid (&reader) with
                    | ValueSome v -> vAltId <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Street1")) then
                    match FsharpJsonHelpers.readString (&reader) with
                    | ValueSome v -> vStreet1 <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Street2")) then
                    match FsharpJsonHelpers.readString (&reader) with
                    | ValueSome v -> vStreet2 <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("IsMonitored")) then
                    match FsharpJsonHelpers.readBoolean (&reader) with
                    | ValueSome v -> vIsMonitored <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Xpos")) then
                    match FsharpJsonHelpers.readSingle (&reader) with
                    | ValueSome v -> vXpos <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Ypos")) then
                    match FsharpJsonHelpers.readDouble (&reader) with
                    | ValueSome v -> vYpos <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Ratio")) then
                    match FsharpJsonHelpers.readMoney (&reader, 2) with
                    | ValueSome v -> vRatio <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("LastChecked")) then
                    match FsharpJsonHelpers.readTimestamp (&reader) with
                    | ValueSome v -> vLastChecked <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("ServiceInterval")) then
                    match FsharpJsonHelpers.readDuration (&reader) with
                    | ValueSome v -> vServiceInterval <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("CurrentLight")) then
                    match
                        FsharpJsonHelpers.readString (&reader)
                        |> ValueOption.map ConvertDomain.TrafficLightFromString
                    with
                    | ValueSome v -> vCurrentLight <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("NicknameValue")) then
                    match FsharpJsonHelpers.readString (&reader) |> ValueOption.map ValueSome with
                    | ValueSome v -> vNickname <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Img")) then
                    match FsharpJsonHelpers.readBytes (&reader) with
                    | ValueSome v -> vImg <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Notes")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.StartArray then
                        while reader.Read() && reader.TokenType <> JsonTokenType.EndArray do
                            match FsharpJsonHelpers.readString (&reader) with
                            | ValueSome v -> vNotes.Add(v)
                            | ValueNone -> ()
                    else
                        reader.Skip()
                else if (reader.ValueTextEquals("Props")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.StartObject then
                        while FsharpJsonHelpers.moveToEndObject (&reader) = false do
                            let propName = reader.GetString()

                            match FsharpJsonHelpers.readString (&reader) with
                            | ValueSome v -> vProps.Add(propName, v)
                            | ValueNone -> ()
                    else
                        reader.Skip()
                else
                    reader.Skip()

        { Id = vId
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
          Props = vProps |> Map.ofSeq }

    static member Crossroad2ToJson(writer: inref<Utf8JsonWriter>, x: Domain.Crossroad2) =
        writer.WriteStartObject()
        writer.WritePropertyName("Id")
        writer.WriteNumberValue(x.Id)
        writer.WritePropertyName("LongId")
        writer.WriteNumberValue(x.LongId)
        writer.WritePropertyName("AltId")
        FsharpJsonHelpers.writeGuid (&writer, x.AltId)
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
        writer.WriteNumberValue(x.Ratio)
        writer.WritePropertyName("LastChecked")
        FsharpJsonHelpers.writeTimestamp (&writer, x.LastChecked)
        writer.WritePropertyName("ServiceInterval")
        FsharpJsonHelpers.writeDuration (&writer, x.ServiceInterval)
        writer.WritePropertyName("CurrentLight")
        writer.WriteStringValue(x.CurrentLight |> ConvertDomain.TrafficLightToString)

        match x.Nickname with
        | ValueSome v ->
            writer.WritePropertyName("NicknameValue")
            writer.WriteStringValue(v)
        | ValueNone -> ()

        writer.WritePropertyName("Img")
        writer.WriteBase64StringValue(System.ReadOnlySpan(x.Img))
        writer.WritePropertyName("Notes")
        writer.WriteStartArray()

        (for v in x.Notes do
            writer.WriteStringValue(v))

        writer.WriteEndArray()
        writer.WritePropertyName("Props")
        writer.WriteStartObject()

        (for pair in x.Props do
            writer.WritePropertyName(pair.Key)
            writer.WriteStringValue(pair.Value))

        writer.WriteEndObject()
        writer.WriteEndObject()
