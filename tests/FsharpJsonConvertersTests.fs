module FsharpJsonConvertersTests

open FSharp.Reflection
open Xunit
open Protogen.Types
open Protogen

type TestData() =
  static member MyTestData =
    [
        ("""module Domain""","""
namespace Protogen.FsharpJsonConverters
open System.Text.Json
open Protogen.FsharpJsonConvertersHelpers
type ConvertDomain () =
""");
        ("""
module Domain
enum TrafficLight =
    | Red
    | Yellow
    | Green
""","""
namespace Protogen.FsharpJsonConverters
open System.Text.Json
open Protogen.FsharpJsonConvertersHelpers
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
""");
        ("""
module Domain
enum TrafficLight =
    | Red
    | Yellow
    | Green
record Crossroad = {
    Id: int
    LongId: long
    AltId: guid
    Street1: string
    Street2: string
    IsMonitored: bool
    Xpos: float
    Ypos: double
    Ratio: decimal(2)
    LastChecked: timestamp
    ServiceInterval: duration
    CurrentLight: TrafficLight
    Nickname: string option
    Img: bytes
    Notes: string array
    Siblings: int list
    Props: string map
}
""","""
namespace Protogen.FsharpJsonConverters
open System.Text.Json
open Protogen.FsharpJsonConvertersHelpers
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
            Siblings = List.empty
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
        let mutable vSiblings = ResizeArray()
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
                else if (reader.ValueTextEquals("Siblings")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.StartArray then
                        while reader.Read() && reader.TokenType <> JsonTokenType.EndArray do
                            if reader.TokenType = JsonTokenType.Number then
                                vSiblings.Add(reader.GetInt32())
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
            Siblings = vSiblings |> List.ofSeq
            Props = vProps |> Map.ofSeq
        }
    static member CrossroadFromJsonDel = lazy(FromJsonDelegate(fun r -> ConvertDomain.CrossroadFromJson(&r)))
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
        writer.WritePropertyName("Siblings")
        writer.WriteStartArray(); (for v in x.Siblings do writer.WriteNumberValue(v)); writer.WriteEndArray()
        writer.WritePropertyName("Props")
        writer.WriteStartObject(); (for pair in x.Props do writer.WritePropertyName(pair.Key); writer.WriteStringValue(pair.Value)); writer.WriteEndObject()
        writer.WriteEndObject()
    static member CrossroadToJsonDel = lazy(ToJsonDelegate(fun w v -> ConvertDomain.CrossroadToJson(&w,v)))

""");
    ("""
module Domain

enum TrafficLight = Red | Yellow | Green

union LightStatus =
    | Normal
    | Warning of errorsCount:int
    | OutOfOrder of since:timestamp*period:duration

record Crossroad = {
    Id: int
    Street1: string
    Street2: string
    CurrentLight: TrafficLight
    LightStatus: LightStatus
}    ""","""
namespace Protogen.FsharpJsonConverters
open System.Text.Json
open Protogen.FsharpJsonConvertersHelpers
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
                    if reader.Read() && reader.TokenType = JsonTokenType.Number
                    then y <- reader.GetInt32() |> Domain.LightStatus.Warning
                    else reader.Skip()
                else if (reader.ValueTextEquals("OutOfOrder")) then
                    y <- ConvertDomain.LightStatusCaseOutOfOrderFromJson(&reader)
                else reader.Skip()
        y
    static member LightStatusFromJsonDel = lazy(FromJsonDelegate(fun r -> ConvertDomain.LightStatusFromJson(&r)))
    static member LightStatusToJson (writer:inref<Utf8JsonWriter>, x: Domain.LightStatus) =
        writer.WriteStartObject()
        match x with
        | Domain.LightStatus.Normal ->
            writer.WritePropertyName("Normal")
            writer.WriteBooleanValue(true)
        | Domain.LightStatus.Warning (errorsCount) ->
            writer.WritePropertyName("Warning")
            writer.WriteNumberValue(errorsCount)
        | Domain.LightStatus.OutOfOrder (since,period) ->
            writer.WritePropertyName("OutOfOrder")
            ConvertDomain.LightStatusCaseOutOfOrderToJson(&writer,since,period)
        | _ ->
            writer.WritePropertyName("Unknown")
            writer.WriteBooleanValue(true)
        writer.WriteEndObject()
    static member LightStatusToJsonDel = lazy(ToJsonDelegate(fun w v -> ConvertDomain.LightStatusToJson(&w,v)))
    static member LightStatusCaseOutOfOrderFromJson (reader: byref<Utf8JsonReader>) =
        let mutable since = System.DateTime.MinValue
        let mutable period = System.TimeSpan.Zero
        if reader.TokenType = JsonTokenType.StartObject || reader.Read() && reader.TokenType = JsonTokenType.StartObject then
            while (reader.Read() && reader.TokenType <> JsonTokenType.EndObject) do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Since")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.String
                    then since <- reader.GetDateTime()
                    else reader.Skip()
                else if (reader.ValueTextEquals("Period")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.String
                    then period <- reader.GetString() |> toTimeSpan
                    else reader.Skip()
                else reader.Skip()
        Domain.LightStatus.OutOfOrder (since,period)
    static member LightStatusCaseOutOfOrderToJson (writer: inref<Utf8JsonWriter>,since,period) =
        writer.WriteStartObject()
        writer.WritePropertyName("Since")
        writer.WriteStringValue(since |> fromDateTime)
        writer.WritePropertyName("Period")
        writer.WriteStringValue(period |> fromTimeSpan)
        writer.WriteEndObject()
    static member DefaultCrossroad: Lazy<Domain.Crossroad> =
        lazy {
            Id = 0
            Street1 = ""
            Street2 = ""
            CurrentLight = ConvertDomain.DefaultTrafficLight.Value
            LightStatus = Domain.LightStatus.Unknown
        }
    static member CrossroadFromJson (reader: byref<Utf8JsonReader>): Domain.Crossroad =
        let mutable vId = 0
        let mutable vStreet1 = ""
        let mutable vStreet2 = ""
        let mutable vCurrentLight = ConvertDomain.DefaultTrafficLight.Value
        let mutable vLightStatus = Domain.LightStatus.Unknown
        if reader.TokenType = JsonTokenType.StartObject || reader.Read() && reader.TokenType = JsonTokenType.StartObject then
            while (reader.Read() && reader.TokenType <> JsonTokenType.EndObject) do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Id")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.Number
                    then vId <- reader.GetInt32()
                    else reader.Skip()
                else if (reader.ValueTextEquals("Street1")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.String
                    then vStreet1 <- reader.GetString()
                    else reader.Skip()
                else if (reader.ValueTextEquals("Street2")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.String
                    then vStreet2 <- reader.GetString()
                    else reader.Skip()
                else if (reader.ValueTextEquals("CurrentLight")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.String
                    then vCurrentLight <- reader.GetString() |> ConvertDomain.TrafficLightFromString
                    else reader.Skip()
                else if (reader.ValueTextEquals("LightStatus")) then
                    vLightStatus <- ConvertDomain.LightStatusFromJson(&reader)
                else reader.Skip()
        {
            Id = vId
            Street1 = vStreet1
            Street2 = vStreet2
            CurrentLight = vCurrentLight
            LightStatus = vLightStatus
        }
    static member CrossroadFromJsonDel = lazy(FromJsonDelegate(fun r -> ConvertDomain.CrossroadFromJson(&r)))
    static member CrossroadToJson (writer: inref<Utf8JsonWriter>, x: Domain.Crossroad) =
        writer.WriteStartObject()
        writer.WritePropertyName("Id")
        writer.WriteNumberValue(x.Id)
        writer.WritePropertyName("Street1")
        writer.WriteStringValue(x.Street1)
        writer.WritePropertyName("Street2")
        writer.WriteStringValue(x.Street2)
        writer.WritePropertyName("CurrentLight")
        writer.WriteStringValue(x.CurrentLight |> ConvertDomain.TrafficLightToString)
        writer.WritePropertyName("LightStatus")
        ConvertDomain.LightStatusToJson(&writer, x.LightStatus)
        writer.WriteEndObject()
    static member CrossroadToJsonDel = lazy(ToJsonDelegate(fun w v -> ConvertDomain.CrossroadToJson(&w,v)))
    """)
    ] |> Seq.map FSharpValue.GetTupleFields

[<Theory; MemberData("MyTestData", MemberType=typeof<TestData>)>]
let testAllCases (input, expectedOutput:string) =
    Parsers.parsePgenDoc input
    |> Result.bind(fun module' ->
        Types.resolveReferences module' []
        |> Result.bind (fun (module',typesCache) ->
            Types.lock module' (LocksCollection []) typesCache
            |> Result.map(fun locks ->
                let outputText = FsharpJsonConvertersCmd.gen module' (LocksCollection locks) typesCache
                Assert.Equal(expectedOutput.Trim(), outputText.Trim())))
        |> Result.mapError(fun error -> failwithf "%A" error))
    |> Result.mapError(fun error -> failwithf "%A" error)