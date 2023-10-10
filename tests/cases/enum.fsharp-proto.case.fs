namespace Test.Converters

type ConvertTestDomain() =

    static member FromProtobuf(x: ProtoClasses.Test.Domain.TrafficLight) : Test.Domain.TrafficLight =
        enum<Test.Domain.TrafficLight> (int x)

    static member ToProtobuf(x: Test.Domain.TrafficLight) : ProtoClasses.Test.Domain.TrafficLight =
        enum<ProtoClasses.Test.Domain.TrafficLight> (int x)

