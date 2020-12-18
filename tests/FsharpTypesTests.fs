module FsharpTypesTests

open FSharp.Reflection
open Xunit
open Protogen.Types
open Protogen

type TestData() =
  static member MyTestData =
    [
        ("""
module Domain
enum TrafficLight =
    | Red
    | Yellow
    | Green ""","""
module rec Domain
open Protogen.FsharpTypes
type TrafficLight =
    | Unknown = 0
    | Red = 1
    | Yellow = 2
    | Green = 3
"""    );
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
    Props: string map
}""","""
module rec Domain
open Protogen.FsharpTypes
type TrafficLight =
    | Unknown = 0
    | Red = 1
    | Yellow = 2
    | Green = 3
type Crossroad = {
    Id : int
    LongId : int64
    AltId : System.Guid
    Street1 : string
    Street2 : string
    IsMonitored : bool
    Xpos : float32
    Ypos : float
    Ratio : decimal
    LastChecked : System.DateTimeOffset
    ServiceInterval : System.TimeSpan
    CurrentLight : Domain.TrafficLight
    Nickname : string option
    Img : byte array
    Notes : string array
    Props : Map<string,string>
}""");
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
}""", """
module rec Domain
open Protogen.FsharpTypes
type TrafficLight =
    | Unknown = 0
    | Red = 1
    | Yellow = 2
    | Green = 3
type LightStatus =
    | Unknown
    | Normal
    | Warning of errorsCount:int
    | OutOfOrder of since:System.DateTimeOffset
type Crossroad = {
    Id : int
    Street1 : string
    Street2 : string
    Light : Domain.TrafficLight
    LightStatus : Domain.LightStatus
}
"""); ("""
module Domain
record Score = {
    S1 : int key
    S2 : int
}
""", """
module rec Domain
open Protogen.FsharpTypes
type Score = {
    S1 : int
    S2 : int
}
with
    static member MakeKey (s1': int) =
        Key.Value (s1'.ToString())
    member x.Key = Score.MakeKey (x.S1)
"""); ("""
module Domain
record Score = {
    S1 : int key
    S2 : int key
}
""", """
module rec Domain
open Protogen.FsharpTypes
type Score = {
    S1 : int
    S2 : int
}
with
    static member MakeKey (s1': int, s2': int) =
        Key.Items [Key.Value (s1'.ToString()); Key.Value (s2'.ToString())]
    member x.Key = Score.MakeKey (x.S1, x.S2)
""");("""
module Domain
enum TrafficLight = Red | Yellow | Green
record Crossroad = {
    Id: int key
    Light: TrafficLight key
    Street1: string
    Street2: string
}
""", """
module rec Domain
open Protogen.FsharpTypes
type TrafficLight =
    | Unknown = 0
    | Red = 1
    | Yellow = 2
    | Green = 3
type Crossroad = {
    Id : int
    Light : Domain.TrafficLight
    Street1 : string
    Street2 : string
}
with
    static member MakeKey (id': int, light': Domain.TrafficLight) =
        Key.Items [Key.Value (id'.ToString()); Key.Value ((int light').ToString())]
    member x.Key = Crossroad.MakeKey (x.Id, x.Light)
"""); ("""
module Domain
record Score = {
    S1 : int key
    S2 : int key
}
record Incident = {
    Score: Score key
    Details: string
}
""", """
module rec Domain
open Protogen.FsharpTypes
type Score = {
    S1 : int
    S2 : int
}
with
    static member MakeKey (s1': int, s2': int) =
        Key.Items [Key.Value (s1'.ToString()); Key.Value (s2'.ToString())]
    member x.Key = Score.MakeKey (x.S1, x.S2)
type Incident = {
    Score : Domain.Score
    Details : string
}
with
    static member MakeKey (scoreKey: Key) =
        Key.Inner scoreKey
    member x.Key = Incident.MakeKey (x.Score.Key)
""");
    ] |> Seq.map FSharpValue.GetTupleFields

[<Theory; MemberData("MyTestData", MemberType=typeof<TestData>)>]
let testAllCases (input, expectedOutput:string) =
    Parsers.parsePgenDoc input
    |> Result.bind(fun module' ->
        Types.resolveReferences module'
        |> Result.mapError(fun error -> failwithf "%A" error)
        |> Result.bind (fun module' ->
            let typesCache = (Types.toTypesCacheItems module' |> Map.ofList)
            Types.lock module' [] typesCache
            |> Result.map(fun locks ->
                let outputText = FsharpTypesCmd.gen module' locks typesCache
                Assert.Equal(expectedOutput.Trim(), outputText.Trim()))
            |> Result.mapError(fun error -> failwithf "%A" error)))
    |> Result.mapError(fun error -> failwithf "%A" error)