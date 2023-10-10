namespace Test.Converters

type ConvertTestDomain() =

    static member FromProtobuf(x: ProtoClasses.Test.Domain.Incident) : Test.Domain.Incident =
        match x.UnionCase with
        | ProtoClasses.Test.Domain.Incident.UnionOneofCase.SwitchedOff -> Test.Domain.Incident.SwitchedOff
        | ProtoClasses.Test.Domain.Incident.UnionOneofCase.MissedTurns -> Test.Domain.Incident.MissedTurns(x.MissedTurns)
        | ProtoClasses.Test.Domain.Incident.UnionOneofCase.Delayes -> x.Delayes |> ConvertTestDomain.FromProtobuf
        | ProtoClasses.Test.Domain.Incident.UnionOneofCase.Root -> x.Root |> ConvertTestDomain.FromProtobuf
        | ProtoClasses.Test.Domain.Incident.UnionOneofCase.Noise -> x.Noise |> ConvertTestDomain.FromProtobuf
        | _ -> Test.Domain.Incident.Unknown
    static member ToProtobuf(x: Test.Domain.Incident) : ProtoClasses.Test.Domain.Incident =
        let y = ProtoClasses.Test.Domain.Incident()
        match x with
        | Test.Domain.Incident.SwitchedOff -> y.SwitchedOff <- true
        | Test.Domain.Incident.MissedTurns (count) ->
            y.MissedTurns <- count
        | Test.Domain.Incident.Delayes (from,to) -> y.Delayes <- ConvertTestDomain.IncidentCaseDelayesToProtobuf(from,to)
        | Test.Domain.Incident.Root (p1) -> y.Root <- ConvertTestDomain.IncidentCaseRootToProtobuf(p1)
        | Test.Domain.Incident.Noise (p1) -> y.Noise <- ConvertTestDomain.IncidentCaseNoiseToProtobuf(p1)
        | Test.Domain.Incident.Unknown -> ()
        y
    static member FromProtobuf (x:ProtoClasses.Test.Domain.Incident__Delayes) =
        Test.Domain.Incident.Delayes
            ((x.From |> fun v -> v.ToDateTime()),(x.To |> fun v -> v.ToDateTime()))

    static member IncidentCaseDelayesToProtobuf (from,to) : ProtoClasses.Test.Domain.Incident__Delayes =
        let y = ProtoClasses.Test.Domain.Incident__Delayes()
        y.From <- from |> Google.Protobuf.WellKnownTypes.Timestamp.FromDateTime
        y.To <- to |> Google.Protobuf.WellKnownTypes.Timestamp.FromDateTime
        y

    static member FromProtobuf (x:ProtoClasses.Test.Domain.Incident__Root) =
        Test.Domain.Incident.Root
            ((x.P1 |> Seq.map(ConvertTestDomain.FromProtobuf) |> List.ofSeq))

    static member IncidentCaseRootToProtobuf (p1) : ProtoClasses.Test.Domain.Incident__Root =
        let y = ProtoClasses.Test.Domain.Incident__Root()
        y.P1.AddRange(p1 |> Seq.map(ConvertTestDomain.ToProtobuf))
        y

    static member FromProtobuf (x:ProtoClasses.Test.Domain.Incident__Noise) =
        Test.Domain.Incident.Noise
            ((x.P1 |> Seq.map(fun x -> x.Key, x.Value) |> Map.ofSeq))

    static member IncidentCaseNoiseToProtobuf (p1) : ProtoClasses.Test.Domain.Incident__Noise =
        let y = ProtoClasses.Test.Domain.Incident__Noise()
        for pair in p1 do
            let protoPair = ProtoClasses.Test.Domain.StringStringPair ()
            protoPair.Key <- pair.Key
            protoPair.Value <- pair.Value
            y.P1.Add(protoPair)
        y


