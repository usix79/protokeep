module FableConvertersTests

open FSharp.Reflection
open Xunit
open Protogen.Types
open Protogen

type TestData() =
  static member MyTestData =
    [
        ("""module Domain""","""
namespace Protogen.FableConverters
open Fable.SimpleJson
open Protogen.FableConverterHelpers
type ConvertDomain () =
""");
        ("""
module Domain
enum TrafficLight =
    | Red
    | Yellow
    | Green
""","""
namespace Protogen.FableConverters
open Fable.SimpleJson
open Protogen.FableConverterHelpers
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
namespace Protogen.FableConverters
open Fable.SimpleJson
open Protogen.FableConverterHelpers
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
    static member CrossroadFromJson (json: Json): Domain.Crossroad =
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
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Id" -> pair.Value |> ifNumber (fun v -> vId <- v |> unbox)
            | "LongId" -> pair.Value |> ifNumber (fun v -> vLongId <- v |> unbox)
            | "AltId" -> pair.Value |> ifString (fun v -> vAltId <- v |> System.Convert.FromBase64String |> System.Guid)
            | "Street1" -> pair.Value |> ifString (fun v -> vStreet1 <- v)
            | "Street2" -> pair.Value |> ifString (fun v -> vStreet2 <- v)
            | "IsMonitored" -> pair.Value |> ifBool (fun v -> vIsMonitored <- v)
            | "Xpos" -> pair.Value |> ifNumber (fun v -> vXpos <- v |> unbox)
            | "Ypos" -> pair.Value |> ifNumber (fun v -> vYpos <- v |> unbox)
            | "Ratio" -> pair.Value |> ifNumber (fun v -> vRatio <- v / 100. |> unbox)
            | "LastChecked" -> pair.Value |> ifString (fun v -> vLastChecked <- v |> toDateTime)
            | "ServiceInterval" -> pair.Value |> ifString (fun v -> vServiceInterval <- v |> toTimeSpan)
            | "CurrentLight" -> pair.Value |> ifString (fun v -> vCurrentLight <- v |> ConvertDomain.TrafficLightFromString)
            | "NicknameValue" -> pair.Value |> ifString (fun v -> vNickname <- v |> Some)
            | "Img" -> pair.Value |> ifString (fun v -> vImg <- v |> System.Convert.FromBase64String)
            | "Notes" -> pair.Value |> ifArray (Seq.iter (ifString (fun v -> v |> vNotes.Add)))
            | "Siblings" -> pair.Value |> ifArray (Seq.iter (ifNumber (fun v -> v |> unbox |> vSiblings.Add)))
            | "Props" -> pair.Value |> ifObject (Map.iter (fun key -> ifString (fun v -> v |> fun v -> vProps.Add(key, v))))
            | _ -> () )
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
            Notes = unbox vNotes
            Siblings = vSiblings |> List.ofSeq
            Props = vProps |> Map.ofSeq
        }
    static member CrossroadToJson (x: Domain.Crossroad) =
        [
           "Id", JNumber (unbox x.Id)
           "LongId", JNumber (unbox x.LongId)
           "AltId", JString (x.AltId.ToByteArray() |> System.Convert.ToBase64String)
           "Street1", JString (x.Street1)
           "Street2", JString (x.Street2)
           "IsMonitored", JBool (x.IsMonitored)
           "Xpos", JNumber (unbox x.Xpos)
           "Ypos", JNumber (unbox x.Ypos)
           "Ratio", JNumber (x.Ratio * 100m |> System.Decimal.Truncate |> unbox)
           "LastChecked", JString (x.LastChecked |> fromDateTime)
           "ServiceInterval", JString (x.ServiceInterval |> fromTimeSpan)
           "CurrentLight", JString (x.CurrentLight |> ConvertDomain.TrafficLightToString)
           match x.Nickname with
           | Some v -> "NicknameValue", JString (v)
           | None -> ()
           "Img", JString (x.Img |> System.Convert.ToBase64String)
           "Notes", JArray (x.Notes |> Seq.map (fun v -> JString (v)) |> List.ofSeq)
           "Siblings", JArray (x.Siblings |> Seq.map (fun v -> JNumber (unbox v)) |> List.ofSeq)
           "Props", JObject (x.Props |> Map.map (fun _ v -> JString (v)))
        ] |> Map.ofList |> JObject
"""); ("""
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
    Light: TrafficLight
    LightStatus: LightStatus
}    ""","""
namespace Protogen.FableConverters
open Fable.SimpleJson
open Protogen.FableConverterHelpers
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
    static member LightStatusFromJson (json: Json): Domain.LightStatus =
        let mutable y = Domain.LightStatus.Unknown
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Normal" -> pair.Value |> ifBool (fun v -> y <- Domain.LightStatus.Normal)
            | "Warning" -> pair.Value |> ifNumber (fun v -> y <- v |> unbox |> Domain.LightStatus.Warning)
            | "OutOfOrder" -> pair.Value |> (fun v -> y <- v |> ConvertDomain.LightStatusCaseOutOfOrderFromJson)
            | _ -> () )
        y
    static member LightStatusToJson (x:Domain.LightStatus) =
        match x with
        | Domain.LightStatus.Normal -> "Normal", JBool (true)
        | Domain.LightStatus.Warning (errorsCount) -> "Warning", JNumber (unbox errorsCount)
        | Domain.LightStatus.OutOfOrder (since,period) -> "OutOfOrder", ConvertDomain.LightStatusCaseOutOfOrderToJson (since,period)
        | _ -> "Unknown", JBool (true)
        |> List.singleton |> Map.ofList |> JObject
    static member LightStatusCaseOutOfOrderFromJson (json: Json) =
        let mutable since = System.DateTime.MinValue
        let mutable period = System.TimeSpan.Zero
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Since" -> pair.Value |> ifString (fun v -> since <- v |> toDateTime)
            | "Period" -> pair.Value |> ifString (fun v -> period <- v |> toTimeSpan)
            | _ -> () )
        Domain.LightStatus.OutOfOrder (since,period)
    static member LightStatusCaseOutOfOrderToJson (since,period) =
        [
           "Since", JString (since |> fromDateTime)
           "Period", JString (period |> fromTimeSpan)
        ] |> Map.ofList |> JObject
    static member DefaultCrossroad: Lazy<Domain.Crossroad> =
        lazy {
            Id = 0
            Street1 = ""
            Street2 = ""
            Light = ConvertDomain.DefaultTrafficLight.Value
            LightStatus = Domain.LightStatus.Unknown
        }
    static member CrossroadFromJson (json: Json): Domain.Crossroad =
        let mutable vId = 0
        let mutable vStreet1 = ""
        let mutable vStreet2 = ""
        let mutable vLight = ConvertDomain.DefaultTrafficLight.Value
        let mutable vLightStatus = Domain.LightStatus.Unknown
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Id" -> pair.Value |> ifNumber (fun v -> vId <- v |> unbox)
            | "Street1" -> pair.Value |> ifString (fun v -> vStreet1 <- v)
            | "Street2" -> pair.Value |> ifString (fun v -> vStreet2 <- v)
            | "Light" -> pair.Value |> ifString (fun v -> vLight <- v |> ConvertDomain.TrafficLightFromString)
            | "LightStatus" -> pair.Value |> (fun v -> vLightStatus <- v |> ConvertDomain.LightStatusFromJson)
            | _ -> () )
        {
            Id = vId
            Street1 = vStreet1
            Street2 = vStreet2
            Light = vLight
            LightStatus = vLightStatus
        }
    static member CrossroadToJson (x: Domain.Crossroad) =
        [
           "Id", JNumber (unbox x.Id)
           "Street1", JString (x.Street1)
           "Street2", JString (x.Street2)
           "Light", JString (x.Light |> ConvertDomain.TrafficLightToString)
           "LightStatus", (x.LightStatus |> ConvertDomain.LightStatusToJson)
        ] |> Map.ofList |> JObject
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
                let outputText = FableConvertersCmd.gen module' (LocksCollection locks) typesCache
                Assert.Equal(expectedOutput.Trim(), outputText.Trim())))
        |> Result.mapError(fun error -> failwithf "%A" error))
    |> Result.mapError(fun error -> failwithf "%A" error)