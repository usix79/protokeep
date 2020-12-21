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
    TrafficLightUnknown = 0;
    TrafficLightRed = 1;
    TrafficLightYellow = 2;
    TrafficLightGreen = 3;
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
    TrafficLightUnknown = 0;
    TrafficLightRed = 1;
    TrafficLightYellow = 2;
    TrafficLightGreen = 3;
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
    TrafficLightUnknown = 0;
    TrafficLightRed = 1;
    TrafficLightYellow = 2;
    TrafficLightGreen = 3;
}
message LightStatus {
    oneof Union {
        bool Normal = 1;
        int32 Warning = 2;
        google.protobuf.Timestamp OutOfOrder = 3;
    }
}
message Crossroad {
    int32 Id = 1;
    string Street1 = 2;
    string Street2 = 3;
    Domain.TrafficLight Light = 4;
    Domain.LightStatus LightStatus = 5;
}
""")
    ] |> Seq.map FSharpValue.GetTupleFields

[<Theory; MemberData("MyTestData", MemberType=typeof<TestData>)>]
let testAllCases (input, expectedOutput:string) =
    Parsers.parsePgenDoc input
    |> Result.bind(fun module' ->
        let typesCache = (Types.toTypesCacheItems module' |> Map.ofList)
        Types.resolveReferences module' []
        |> Result.mapError (fun error -> failwithf "%A" error)
        |> Result.map (fun (module', typesCache) ->
            Types.lock module' (LocksCollection []) typesCache
            |> Result.map(fun locks ->
                let outputText = ProtoCmd.gen module' (LocksCollection locks) typesCache
                Assert.Equal(expectedOutput.Trim(), outputText.Trim()))
            |> Result.mapError(fun error -> failwithf "%A" error)))
    |> Result.mapError(fun error -> failwithf "%A" error)