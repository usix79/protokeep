namespace Test.Converters

open System.Text.Json
open Protokeep

type ConvertTestDomain() =

    static member IncidentFromJson(reader: byref<Utf8JsonReader>): Test.Domain.Incident voption =
        if FsharpJsonHelpers.moveToStartObject(&reader)then
            let mutable y = Test.Domain.Incident.Unknown
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("SwitchedOff")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.True
                    then y <- Test.Domain.Incident.SwitchedOff
                    else reader.Skip()
                else if (reader.ValueTextEquals("MissedTurns")) then
                    let mutable _count = 0
                    match FsharpJsonHelpers.readInt(&reader) with
                    | ValueSome v -> _count <- v
                    | ValueNone -> ()
                    y <- _count |> Test.Domain.Incident.MissedTurns
                else if (reader.ValueTextEquals("Delayes")) then
                    y <- ConvertTestDomain.IncidentCaseDelayesFromJson(&reader)
                else if (reader.ValueTextEquals("Root")) then
                    let mutable _p1 = ResizeArray()
                    if reader.Read() && reader.TokenType = JsonTokenType.StartArray then
                        while reader.TokenType <> JsonTokenType.EndArray do
                            match ConvertTestDomain.IncidentFromJson(&reader) with
                            | ValueSome v -> _p1.Add(v)
                            | ValueNone -> ()
                    else reader.Skip()
                    y <- _p1 |> List.ofSeq |> Test.Domain.Incident.Root
                else if (reader.ValueTextEquals("Noise")) then
                    let mutable _p1 = ResizeArray()
                    if reader.Read() && reader.TokenType = JsonTokenType.StartArray then
                        while reader.Read() && reader.TokenType <> JsonTokenType.EndArray do
                            if reader.TokenType = JsonTokenType.StartObject then
                                let mutable key = ""
                                let mutable value = ""
                                while reader.Read() && reader.TokenType <> JsonTokenType.EndObject do
                                    if (reader.ValueTextEquals("Key")) then
                                        match FsharpJsonHelpers.readString(&reader) with
                                        | ValueSome v -> key <- v
                                        | ValueNone -> ()
                                    else if (reader.ValueTextEquals("Value")) then
                                        match FsharpJsonHelpers.readString(&reader) with
                                        | ValueSome v -> value <- v
                                        | ValueNone -> ()
                                    else reader.Skip()
                                _p1.Add(key, value)
                            else reader.Skip()
                    else reader.Skip()
                    y <- _p1 |> Map.ofSeq |> Test.Domain.Incident.Noise
                else reader.Skip()
            ValueSome y
        else ValueNone
    static member IncidentToJson (writer:inref<Utf8JsonWriter>, x: Test.Domain.Incident) =
        writer.WriteStartObject()
        match x with
        | Test.Domain.Incident.SwitchedOff ->
            writer.WritePropertyName("SwitchedOff")
            writer.WriteBooleanValue(true)
        | Test.Domain.Incident.MissedTurns (count) ->
            writer.WritePropertyName("MissedTurns")
            writer.WriteNumberValue(count)
        | Test.Domain.Incident.Delayes (from,to) ->
            writer.WritePropertyName("Delayes")
            ConvertTestDomain.IncidentCaseDelayesToJson(&writer,from,to)
        | Test.Domain.Incident.Root (p1) ->
            writer.WritePropertyName("Root")
            writer.WriteStartArray()
            for v in p1 do
                ConvertTestDomain.IncidentToJson(&writer, v)
            writer.WriteEndArray()
        | Test.Domain.Incident.Noise (p1) ->
            writer.WritePropertyName("Noise")
            writer.WriteStartArray()
            for pair in p1 do
                writer.WriteStartObject()
                writer.WritePropertyName("Key")
                writer.WriteStringValue(pair.Key)
                writer.WritePropertyName("Value")
                writer.WriteStringValue(pair.Value)
                writer.WriteEndObject()
            writer.WriteEndArray()
        | _ ->
            writer.WritePropertyName("Unknown")
            writer.WriteBooleanValue(true)
        writer.WriteEndObject()
    static member IncidentCaseDelayesFromJson(reader: byref<Utf8JsonReader>) =
        let mutable from = System.DateTime.MinValue
        let mutable to = System.DateTime.MinValue
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("From")) then
                    match FsharpJsonHelpers.readTimestamp(&reader) with
                    | ValueSome v -> from <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("To")) then
                    match FsharpJsonHelpers.readTimestamp(&reader) with
                    | ValueSome v -> to <- v
                    | ValueNone -> ()
                else reader.Skip()
        Test.Domain.Incident.Delayes (from,to)
    static member IncidentCaseDelayesToJson (writer: inref<Utf8JsonWriter>,from,to) =
        writer.WriteStartObject()
        writer.WritePropertyName("From")
        FsharpJsonHelpers.writeTimestamp(&writer, from)
        writer.WritePropertyName("To")
        FsharpJsonHelpers.writeTimestamp(&writer, to)
        writer.WriteEndObject()

