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
option csharp_namespace = "ProtoClasses.Domain";
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
option csharp_namespace = "ProtoClasses.Domain";
import "google/protobuf/timestamp.proto";
import "google/protobuf/duration.proto";
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
option csharp_namespace = "ProtoClasses.Domain";
import "google/protobuf/timestamp.proto";
enum TrafficLight {
    Unknown = 0;
    Red = 1;
    Yellow = 2;
    Green = 3;
}
message Crossroad {
    int32 Id = 1;
    string Street1 = 2;
    string Street2 = 3;
    Domain.TrafficLight Light = 4;
    oneof LightStatus {
        bool LightStatusNormal = 5;
        int32 LightStatusWarning = 6;
        google.protobuf.Timestamp LightStatusOutOfOrder = 7;
    }
}
""")
    ] |> Seq.map FSharpValue.GetTupleFields

[<Theory; MemberData("MyTestData", MemberType=typeof<TestData>)>]
let testAllCases (input, expectedOutput:string) =
    Parsers.parsePgenDoc input
    |> Result.bind(fun module' ->
        Types.lock module' []
        |> Result.map(fun locks ->
            let outputText = ProtoCmd.gen module' locks
            Assert.Equal(expectedOutput.Trim(), outputText.Trim()))
        |> Result.mapError(fun error -> failwithf "%A" error))
    |> Result.mapError(fun error -> failwithf "%A" error)