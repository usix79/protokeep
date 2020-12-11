module ProtoTests

open System
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
syntax = "proto3";
package Domain;
enum TrafficLight {
    Unknown = 0;
    Red = 1;
    Yellow = 2;
    Green = 3;
}"""    );
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
syntax = "proto3";
package Domain;
import "google.protobuf.timestamp.proto";
import "google.protobuf.duration.proto";
enum TrafficLight {
    Unknown = 0;
    Red = 1;
    Yellow = 2;
    Green = 3;
}
message Crossroad {
    int32 Id = 1;
    int64 LongId = 2;
    bytes AltId = 3;
    string Street1 = 4;
    string Street2 = 5;
    bool IsMonitored = 6;
    float Xpos = 7;
    double Ypos = 8;
    int64 Ratio = 9;
    google.protobuf.Timestamp LastChecked = 10;
    google.protobuf.Duration ServiceInterval = 11;
    Domain.TrafficLight CurrentLight = 12;
    oneof Nickname {string NicknameValue = 13;}
    bytes Img = 14;
    repeated string Notes = 15;
    map<string,string> Props = 16;
}
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
}""", """
syntax = "proto3";
package Domain;
import "google.protobuf.timestamp.proto";
enum TrafficLight {
    Unknown = 0;
    Red = 1;
    Yellow = 2;
    Green = 3;
}
message LightStatus__Normal {
}
message LightStatus__Warning {
    int32 ErrorsCount = 1;
}
message LightStatus__OutOfOrder {
    google.protobuf.Timestamp Since = 1;
}
message Crossroad {
    int32 Id = 1;
    string Street1 = 2;
    string Street2 = 3;
    Domain.TrafficLight Light = 4;
    oneof LightStatus {
        Domain.LightStatus__Normal LightStatusNormal = 5;
        Domain.LightStatus__Warning LightStatusWarning = 6;
        Domain.LightStatus__OutOfOrder LightStatusOutOfOrder = 7;
    }
}
""")
    ] |> Seq.map FSharpValue.GetTupleFields

[<Theory; MemberData("MyTestData", MemberType=typeof<TestData>)>]
let testAllCases (input, expectedOutput:string) =
    Parsers.parsePgenDoc input
    |> Result.bind(fun modules ->
        Types.lock modules []
        |> Result.map(fun locks ->
            let outputText = snd (ProtoCmd.gen modules locks).Head
            Assert.Equal(expectedOutput.Trim(), outputText.Trim()))
        |> Result.mapError(fun error -> failwithf "%A" error))
    |> Result.mapError(fun error -> failwithf "%A" error)