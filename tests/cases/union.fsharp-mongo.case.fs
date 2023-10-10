namespace Test.Converters

open MongoDB.Bson
open MongoDB.Bson.IO
open MongoDB.Bson.Serialization
open MongoDB.Bson.Serialization.Serializers
open Protokeep

type ConvertTestDomain() =
    static member IncidentToBson(writer: IBsonWriter, x: Test.Domain.Incident) =
        writer.WriteStartDocument()
        match x with
        | Test.Domain.Incident.SwitchedOff ->
            writer.WriteName("SwitchedOff")
            writer.WriteBoolean(true)
        | Test.Domain.Incident.MissedTurns (count) ->
            writer.WriteName("MissedTurns")
            writer.WriteInt32(count)
        | Test.Domain.Incident.Delayes (from,to) ->
            writer.WriteName("Delayes")
            ConvertTestDomain.IncidentCaseDelayesToBson(writer,from,to)
        | Test.Domain.Incident.Root (p1) ->
            writer.WriteName("Root")
            writer.WriteStartArray()
            for v in p1 do
                ConvertTestDomain.IncidentToBson(writer, v)
            writer.WriteEndArray()
        | Test.Domain.Incident.Noise (p1) ->
            writer.WriteName("Noise")
            writer.WriteStartArray()
            for pair in p1 do
                writer.WriteStartDocument()
                writer.WriteName("Key")
                writer.WriteString(pair.Key)
                writer.WriteName("Value")
                writer.WriteString(pair.Value)
                writer.WriteEndDocument()
            writer.WriteEndArray()
        | _ ->
            writer.WriteName("Unknown")
            writer.WriteBoolean(true)
        writer.WriteEndDocument()
    static member IncidentCaseDelayesToBson (writer: IBsonWriter,from,to) =
        writer.WriteStartDocument()
        writer.WriteName("From")
        writer.WriteDateTime(FsharpMongoHelpers.fromDateTime from)
        writer.WriteName("To")
        writer.WriteDateTime(FsharpMongoHelpers.fromDateTime to)
        writer.WriteEndDocument()
    static member IncidentFromBson (reader: IBsonReader): Test.Domain.Incident =
        let mutable y = Test.Domain.Incident.Unknown
        reader.ReadStartDocument()
        match reader.ReadName() with
                | "SwitchedOff" ->
                    match FsharpMongoHelpers.readBoolean reader with
                    | ValueSome true -> y <- Test.Domain.Incident.SwitchedOff
                    | _ -> ()
                | "MissedTurns" ->
                    let mutable _count = 0
                    match FsharpMongoHelpers.readInt reader with
                    | ValueSome v -> _count <- v
                    | ValueNone -> ()
                    y <- _count |> Test.Domain.Incident.MissedTurns
                | "Delayes" ->
                    y <- ConvertTestDomain.IncidentCaseDelayesFromBson(reader)
                | "Root" ->
                    let mutable _p1 = ResizeArray()
                    reader.ReadStartArray()
                    while reader.ReadBsonType() <> BsonType.EndOfDocument do
                        match ConvertTestDomain.IncidentFromBson(reader) |> ValueSome with
                        | ValueSome v -> _p1.Add(v)
                        | ValueNone -> ()
                    reader.ReadEndArray()
                    y <- _p1 |> List.ofSeq |> Test.Domain.Incident.Root
                | "Noise" ->
                    let mutable _p1 = ResizeArray()
                    reader.ReadStartArray()
                    while reader.ReadBsonType() <> BsonType.EndOfDocument do
                        reader.ReadStartDocument()
                        let mutable key = ""
                        let mutable value = ""
                        while reader.ReadBsonType() <> BsonType.EndOfDocument do
                            match reader.ReadName() with
                            | "Key" ->
                                match FsharpMongoHelpers.readString reader with
                                | ValueSome v -> key <- v
                                | ValueNone -> ()
                            | "Value" ->
                                match FsharpMongoHelpers.readString reader with
                                | ValueSome v -> value <- v
                                | ValueNone -> ()
                            | _ -> reader.SkipValue()
                        reader.ReadEndDocument()
                        _p1.Add(key, value)
                    reader.ReadEndArray()
                    y <- _p1 |> Map.ofSeq |> Test.Domain.Incident.Noise
                | _ -> ()
        reader.ReadEndDocument()
        y
    static member IncidentCaseDelayesFromBson (reader: IBsonReader) =
        let mutable from = System.DateTime.MinValue
        let mutable to = System.DateTime.MinValue
        reader.ReadStartDocument()
        while reader.State <> BsonReaderState.EndOfDocument do
            match reader.State with
            | BsonReaderState.Type -> reader.ReadBsonType() |> ignore
            | BsonReaderState.Name ->
                match reader.ReadName() with
                | "From" ->
                    match FsharpMongoHelpers.readTimestamp reader with
                    | ValueSome v -> from <- v
                    | ValueNone -> ()
                | "To" ->
                    match FsharpMongoHelpers.readTimestamp reader with
                    | ValueSome v -> to <- v
                    | ValueNone -> ()
                | _ -> reader.SkipValue()
            | _ -> printfn "Unexpected state: %A" reader.State
        reader.ReadEndDocument()
        Test.Domain.Incident.Delayes (from,to)

type ConvertTestDomain with
    static member RegisterSerializers() =
        BsonDefaults.GuidRepresentationMode <- GuidRepresentationMode.V3
