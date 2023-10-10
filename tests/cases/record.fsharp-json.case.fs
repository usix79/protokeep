namespace Test.Converters

open System.Text.Json
open Protokeep

type ConvertTestDomain() =

    static member CrossroadToJson (writer: inref<Utf8JsonWriter>, x: Test.Domain.Crossroad) =
        writer.WriteStartObject()
        writer.WritePropertyName("Id")
        writer.WriteNumberValue(x.Id)
        writer.WritePropertyName("AltId")
        FsharpJsonHelpers.writeGuid(&writer, x.AltId)
        writer.WritePropertyName("Address")
        writer.WriteStringValue(x.Address)
        match x.Corner with
        | ValueSome v ->
            writer.WritePropertyName("CornerValue")
            writer.WriteStringValue(v)
        | ValueNone -> ()
        writer.WritePropertyName("IsMonitored")
        writer.WriteBooleanValue(x.IsMonitored)
        writer.WritePropertyName("Patch")
        writer.WriteNumberValue(x.Patch)
        writer.WritePropertyName("Model")
        writer.WriteNumberValue(x.Model)
        writer.WritePropertyName("Serial")
        writer.WriteNumberValue(x.Serial)
        writer.WritePropertyName("Mask")
        writer.WriteNumberValue(x.Mask)
        writer.WritePropertyName("Cost")
        writer.WriteNumberValue(x.Cost)
        writer.WritePropertyName("Xpos")
        writer.WriteNumberValue(x.Xpos)
        writer.WritePropertyName("Ypos")
        writer.WriteNumberValue(x.Ypos)
        writer.WritePropertyName("LastChecked")
        FsharpJsonHelpers.writeTimestamp(&writer, x.LastChecked)
        writer.WritePropertyName("ServiceInterval")
        FsharpJsonHelpers.writeDuration(&writer, x.ServiceInterval)
        writer.WritePropertyName("Intervals")
        writer.WriteStartArray()
        for v in x.Intervals do
            writer.WriteNumberValue(v)
        writer.WriteEndArray()
        writer.WritePropertyName("Notes")
        writer.WriteStartArray()
        for v in x.Notes do
            writer.WriteStringValue(v)
        writer.WriteEndArray()
        writer.WritePropertyName("Tags")
        writer.WriteStartArray()
        for pair in x.Tags do
            writer.WriteStartObject()
            writer.WritePropertyName("Key")
            writer.WriteStringValue(pair.Key)
            writer.WritePropertyName("Value")
            writer.WriteNumberValue(pair.Value)
            writer.WriteEndObject()
        writer.WriteEndArray()
        match x.Next with
        | ValueSome v ->
            writer.WritePropertyName("NextValue")
            ConvertTestDomain.CrossroadToJson(&writer, v)
        | ValueNone -> ()
        writer.WritePropertyName("Img")
        writer.WriteBase64StringValue(System.ReadOnlySpan(x.Img))
        writer.WritePropertyName("Version")
        writer.WriteNumberValue(x.Version)
        writer.WriteEndObject()

    static member CrossroadFromJson(reader: byref<Utf8JsonReader>): Test.Domain.Crossroad voption =
        let mutable vId = 0
        let mutable vAltId = System.Guid.Empty
        let mutable vAddress = ""
        let mutable vCorner = ValueNone
        let mutable vIsMonitored = false
        let mutable vPatch = 0uy
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
        let mutable vNext = ValueNone
        let mutable vImg = Array.empty
        let mutable vVersion = 0
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Id")) then
                    match FsharpJsonHelpers.readInt(&reader) with
                    | ValueSome v -> vId <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("AltId")) then
                    match FsharpJsonHelpers.readGuid(&reader) with
                    | ValueSome v -> vAltId <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Address")) then
                    match FsharpJsonHelpers.readString(&reader) with
                    | ValueSome v -> vAddress <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("CornerValue")) then
                    match FsharpJsonHelpers.readString(&reader) |> ValueOption.map ValueSome with
                    | ValueSome v -> vCorner <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("IsMonitored")) then
                    match FsharpJsonHelpers.readBoolean(&reader) with
                    | ValueSome v -> vIsMonitored <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Patch")) then
                    match FsharpJsonHelpers.readByte(&reader) with
                    | ValueSome v -> vPatch <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Model")) then
                    match FsharpJsonHelpers.readShort(&reader) with
                    | ValueSome v -> vModel <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Serial")) then
                    match FsharpJsonHelpers.readInt(&reader) with
                    | ValueSome v -> vSerial <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Mask")) then
                    match FsharpJsonHelpers.readLong(&reader) with
                    | ValueSome v -> vMask <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Cost")) then
                    match FsharpJsonHelpers.readMoney(&reader, 2) with
                    | ValueSome v -> vCost <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Xpos")) then
                    match FsharpJsonHelpers.readSingle(&reader) with
                    | ValueSome v -> vXpos <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Ypos")) then
                    match FsharpJsonHelpers.readDouble(&reader) with
                    | ValueSome v -> vYpos <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("LastChecked")) then
                    match FsharpJsonHelpers.readTimestamp(&reader) with
                    | ValueSome v -> vLastChecked <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("ServiceInterval")) then
                    match FsharpJsonHelpers.readDuration(&reader) with
                    | ValueSome v -> vServiceInterval <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Intervals")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.StartArray then
                        while reader.TokenType <> JsonTokenType.EndArray do
                            match FsharpJsonHelpers.readInt(&reader) with
                            | ValueSome v -> vIntervals.Add(v)
                            | ValueNone -> ()
                    else reader.Skip()
                else if (reader.ValueTextEquals("Notes")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.StartArray then
                        while reader.TokenType <> JsonTokenType.EndArray do
                            match FsharpJsonHelpers.readString(&reader) with
                            | ValueSome v -> vNotes.Add(v)
                            | ValueNone -> ()
                    else reader.Skip()
                else if (reader.ValueTextEquals("Tags")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.StartArray then
                        while reader.Read() && reader.TokenType <> JsonTokenType.EndArray do
                            if reader.TokenType = JsonTokenType.StartObject then
                                let mutable key = ""
                                let mutable value = 0
                                while reader.Read() && reader.TokenType <> JsonTokenType.EndObject do
                                    if (reader.ValueTextEquals("Key")) then
                                        match FsharpJsonHelpers.readString(&reader) with
                                        | ValueSome v -> key <- v
                                        | ValueNone -> ()
                                    else if (reader.ValueTextEquals("Value")) then
                                        match FsharpJsonHelpers.readInt(&reader) with
                                        | ValueSome v -> value <- v
                                        | ValueNone -> ()
                                    else reader.Skip()
                                vTags.Add(key, value)
                            else reader.Skip()
                    else reader.Skip()
                else if (reader.ValueTextEquals("NextValue")) then
                    match ConvertTestDomain.CrossroadFromJson(&reader) |> ValueOption.map ValueSome with
                    | ValueSome v -> vNext <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Img")) then
                    match FsharpJsonHelpers.readBytes(&reader) with
                    | ValueSome v -> vImg <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Version")) then
                    match FsharpJsonHelpers.readInt(&reader) with
                    | ValueSome v -> vVersion <- v
                    | ValueNone -> ()
                else reader.Skip()
            ValueSome {
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
                Tags = vTags |> Map.ofSeq
                Next = vNext
                Img = vImg
                Version = vVersion
            }
        else ValueNone
