module FsharpConvertersTests

open FSharp.Reflection
open Xunit
open Protogen.Types
open Protogen

type TestData() =
  static member MyTestData =
    [
        ("""module Domain""","""
namespace ProtoConverters.FsharpTypes
type ConvertDomain () =
""");
        ("""
module Domain
enum TrafficLight =
    | Red
    | Yellow
    | Green
""","""
namespace ProtoConverters.FsharpTypes
type ConvertDomain () =
    static member FromProtobuf (x:ProtoClasses.Domain.TrafficLight) : Domain.TrafficLight =
        enum<Domain.TrafficLight>(int x)
    static member ToProtobuf (x:Domain.TrafficLight) : ProtoClasses.Domain.TrafficLight =
        enum<ProtoClasses.Domain.TrafficLight>(int x)
""");
        ("""
module Domain
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
    Nickname: string option
    Img: bytes
    Notes: string array
    Props: string map
}
""","""
namespace ProtoConverters.FsharpTypes
type ConvertDomain () =
    static member FromProtobuf (x:ProtoClasses.Domain.Crossroad) : Domain.Crossroad =
        {
            Id = x.Id
            LongId = x.LongId
            AltId = x.AltId |> fun v -> System.Guid(v.ToByteArray())
            Street1 = x.Street1
            Street2 = x.Street2
            IsMonitored = x.IsMonitored
            Xpos = x.Xpos
            Ypos = x.Ypos
            Ratio = x.Ratio |> fun v -> (decimal v) / 100m
            LastChecked = x.LastChecked |> fun v -> v.ToDateTimeOffset()
            ServiceInterval = x.ServiceInterval |> fun v -> v.ToTimeSpan()
            Nickname = if x.NicknameCase = ProtoClasses.Domain.Crossroad.NicknameOneofCase.NicknameValue then Some (x.NicknameValue) else None
            Img = x.Img |> fun v -> v.ToByteArray()
            Notes = x.Notes |> Array.ofSeq
            Props = x.Props |> Seq.map(fun pair -> pair.Key,pair.Value) |> Map.ofSeq
        }
    static member ToProtobuf (x:Domain.Crossroad) : ProtoClasses.Domain.Crossroad =
        let y = ProtoClasses.Domain.Crossroad()
        y.Id <- x.Id
        y.LongId <- x.LongId
        y.AltId <- x.AltId |> fun v -> Google.Protobuf.ByteString.CopyFrom(v.ToByteArray())
        y.Street1 <- x.Street1
        y.Street2 <- x.Street2
        y.IsMonitored <- x.IsMonitored
        y.Xpos <- x.Xpos
        y.Ypos <- x.Ypos
        y.Ratio <- x.Ratio |> fun v -> int64(v * 100m)
        y.LastChecked <- x.LastChecked |> Google.Protobuf.WellKnownTypes.Timestamp.FromDateTimeOffset
        y.ServiceInterval <- x.ServiceInterval |> Google.Protobuf.WellKnownTypes.Duration.FromTimeSpan
        match x.Nickname with
        | Some v -> y.NicknameValue <- v
        | None -> ()
        y.Img <- x.Img |> Google.Protobuf.ByteString.CopyFrom
        y.Notes.AddRange(x.Notes)
        y.Props.Add(x.Props)
        y
""");
    ("""
module Domain

enum TrafficLight = Red | Yellow | Green

union LightStatus =
    | Normal
    | Warning of errorsCount:int
    | OutOfOrder of since:timestamp

record Crossroad = {
    Id: int
    Street1: string
    Street2: string
    Light: TrafficLight
    LightStatus: LightStatus
}    ""","""
namespace ProtoConverters.FsharpTypes
type ConvertDomain () =
    static member FromProtobuf (x:ProtoClasses.Domain.TrafficLight) : Domain.TrafficLight =
        enum<Domain.TrafficLight>(int x)
    static member ToProtobuf (x:Domain.TrafficLight) : ProtoClasses.Domain.TrafficLight =
        enum<ProtoClasses.Domain.TrafficLight>(int x)
    static member FromProtobuf (x:ProtoClasses.Domain.LightStatus__Normal)  =
        Domain.LightStatus.Normal
    static member ToProtobufLightStatusCaseNormal () : ProtoClasses.Domain.LightStatus__Normal =
        let y = ProtoClasses.Domain.LightStatus__Normal()
        y
    static member FromProtobuf (x:ProtoClasses.Domain.LightStatus__Warning)  =
        Domain.LightStatus.Warning
            ((x.ErrorsCount))
    static member ToProtobufLightStatusCaseWarning (errorsCount) : ProtoClasses.Domain.LightStatus__Warning =
        let y = ProtoClasses.Domain.LightStatus__Warning()
        y.ErrorsCount <- errorsCount
        y
    static member FromProtobuf (x:ProtoClasses.Domain.LightStatus__OutOfOrder)  =
        Domain.LightStatus.OutOfOrder
            ((x.Since |> fun v -> v.ToDateTimeOffset()))
    static member ToProtobufLightStatusCaseOutOfOrder (since) : ProtoClasses.Domain.LightStatus__OutOfOrder =
        let y = ProtoClasses.Domain.LightStatus__OutOfOrder()
        y.Since <- since |> Google.Protobuf.WellKnownTypes.Timestamp.FromDateTimeOffset
        y
    static member FromProtobuf (x:ProtoClasses.Domain.Crossroad) : Domain.Crossroad =
        {
            Id = x.Id
            Street1 = x.Street1
            Street2 = x.Street2
            Light = x.Light |> ConvertDomain.FromProtobuf
            LightStatus = """ + """
                match x.LightStatusCase with
                | ProtoClasses.Domain.Crossroad.LightStatusOneofCase.LightStatusNormal -> x.LightStatusNormal |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Crossroad.LightStatusOneofCase.LightStatusWarning -> x.LightStatusWarning |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Crossroad.LightStatusOneofCase.LightStatusOutOfOrder -> x.LightStatusOutOfOrder |> ConvertDomain.FromProtobuf
                | _ -> Domain.LightStatus.Unknown
        }
    static member ToProtobuf (x:Domain.Crossroad) : ProtoClasses.Domain.Crossroad =
        let y = ProtoClasses.Domain.Crossroad()
        y.Id <- x.Id
        y.Street1 <- x.Street1
        y.Street2 <- x.Street2
        y.Light <- x.Light |> ConvertDomain.ToProtobuf
        match x.LightStatus with
        | Domain.LightStatus.Normal -> y.LightStatusNormal <- ConvertDomain.ToProtobufLightStatusCaseNormal()
        | Domain.LightStatus.Warning (errorsCount) -> y.LightStatusWarning <- ConvertDomain.ToProtobufLightStatusCaseWarning(errorsCount)
        | Domain.LightStatus.OutOfOrder (since) -> y.LightStatusOutOfOrder <- ConvertDomain.ToProtobufLightStatusCaseOutOfOrder(since)
        | Domain.LightStatus.Unknown -> ()
        y
    """)
    ] |> Seq.map FSharpValue.GetTupleFields

[<Theory; MemberData("MyTestData", MemberType=typeof<TestData>)>]
let testAllCases (input, expectedOutput:string) =
    Parsers.parsePgenDoc input
    |> Result.bind(fun modules ->
        Types.lockInternal modules []
        |> Result.map(fun (locks, typesCache) ->
            let outputText = FsharpConvertersCmd.gen modules locks typesCache
            Assert.Equal(expectedOutput.Trim(), outputText.Trim()))
        |> Result.mapError(fun error -> failwithf "%A" error))
    |> Result.mapError(fun error -> failwithf "%A" error)