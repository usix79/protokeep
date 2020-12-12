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
module Domain
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
module Domain
type TrafficLight =
    | Unknown = 0
    | Red = 1
    | Yellow = 2
    | Green = 3
type Crossroad = {
    Id : int
    LongId : long
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
module Domain
type TrafficLight =
    | Unknown = 0
    | Red = 1
    | Yellow = 2
    | Green = 3
type LightStatus =
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
""")
    ] |> Seq.map FSharpValue.GetTupleFields

[<Theory; MemberData("MyTestData", MemberType=typeof<TestData>)>]
let testAllCases (input, expectedOutput:string) =
    Parsers.parsePgenDoc input
    |> Result.bind(fun modules ->
        Types.lockInternal modules []
        |> Result.map(fun (locks, typesCache) ->
            let outputText = FsharpTypesCmd.gen modules locks typesCache
            Assert.Equal(expectedOutput.Trim(), outputText.Trim()))
        |> Result.mapError(fun error -> failwithf "%A" error))
    |> Result.mapError(fun error -> failwithf "%A" error)