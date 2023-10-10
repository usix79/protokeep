namespace Test.Converters

open Fable.SimpleJson
open Protokeep

type ConvertTestDomain () =
    static member TrafficLightFromString = function
        | "TrafficLightRed" -> Test.Domain.TrafficLight.Red
        | "TrafficLightYellow" -> Test.Domain.TrafficLight.Yellow
        | "TrafficLightGreen" -> Test.Domain.TrafficLight.Green
        | _ -> Test.Domain.TrafficLight.Unknown
    static member TrafficLightToString = function
        | Test.Domain.TrafficLight.Red -> "TrafficLightRed"
        | Test.Domain.TrafficLight.Yellow -> "TrafficLightYellow"
        | Test.Domain.TrafficLight.Green -> "TrafficLightGreen"
        | _ -> "Unknown"
