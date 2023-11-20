namespace Test.Converters

open System.Text.Json
open Protokeep

type ConvertTestDomain() =

    static member TrafficLightFromString =
        function
        | "Red" -> Test.Domain.TrafficLight.Red
        | "Yellow" -> Test.Domain.TrafficLight.Yellow
        | "Green" -> Test.Domain.TrafficLight.Green
        | _ -> Test.Domain.TrafficLight.Unknown

    static member TrafficLightToString =
        function
        | Test.Domain.TrafficLight.Red -> "Red"
        | Test.Domain.TrafficLight.Yellow -> "Yellow"
        | Test.Domain.TrafficLight.Green -> "Green"
        | _ -> "Unknown"

