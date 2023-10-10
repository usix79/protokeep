namespace Test.Converters

open MongoDB.Bson
open MongoDB.Bson.IO
open MongoDB.Bson.Serialization
open MongoDB.Bson.Serialization.Serializers
open Protokeep

type ConvertTestDomain() =
    static memberTrafficLightFromInt = function
        | Test.Domain.TrafficLight.Red -> Test.Domain.TrafficLight.Red
        | Test.Domain.TrafficLight.Yellow -> Test.Domain.TrafficLight.Yellow
        | Test.Domain.TrafficLight.Green -> Test.Domain.TrafficLight.Green
        | _ -> Test.Domain.TrafficLight.Unknown


type ConvertTestDomain with
    static member RegisterSerializers() =
        BsonDefaults.GuidRepresentationMode <- GuidRepresentationMode.V3
