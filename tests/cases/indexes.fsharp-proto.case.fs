namespace Test.Converters

type ConvertTestDomain() =

    static member FromProtobuf(x: ProtoClasses.Test.Domain.ThreeWaysMarket) : Test.Domain.ThreeWaysMarket =
        {
            Win1 = x.Win1 |> fun v -> (decimal v) / 1000m
            Draw = x.Draw |> fun v -> (decimal v) / 1000m
            Win2 = x.Win2 |> fun v -> (decimal v) / 1000m
        }

    static member ToProtobuf(x: Test.Domain.ThreeWaysMarket) : ProtoClasses.Test.Domain.ThreeWaysMarket =
        let y = ProtoClasses.Test.Domain.ThreeWaysMarket()
        y.Win1 <- x.Win1 |> fun v -> int (v * 1000m)
        y.Draw <- x.Draw |> fun v -> int (v * 1000m)
        y.Win2 <- x.Win2 |> fun v -> int (v * 1000m)
        y

    static member FromProtobuf(x: ProtoClasses.Test.Domain.TwoWaysMarket) : Test.Domain.TwoWaysMarket =
        {
            Win1 = x.Win1 |> fun v -> (decimal v) / 1000m
            Win2 = x.Win2 |> fun v -> (decimal v) / 1000m
        }

    static member ToProtobuf(x: Test.Domain.TwoWaysMarket) : ProtoClasses.Test.Domain.TwoWaysMarket =
        let y = ProtoClasses.Test.Domain.TwoWaysMarket()
        y.Win1 <- x.Win1 |> fun v -> int (v * 1000m)
        y.Win2 <- x.Win2 |> fun v -> int (v * 1000m)
        y

    static member FromProtobuf(x: ProtoClasses.Test.Domain.HandicapMarket) : Test.Domain.HandicapMarket =
        {
            Value = x.Value |> fun v -> (decimal v) / 100m
            Win1 = x.Win1 |> fun v -> (decimal v) / 1000m
            Win2 = x.Win2 |> fun v -> (decimal v) / 1000m
        }

    static member ToProtobuf(x: Test.Domain.HandicapMarket) : ProtoClasses.Test.Domain.HandicapMarket =
        let y = ProtoClasses.Test.Domain.HandicapMarket()
        y.Value <- x.Value |> fun v -> int (v * 100m)
        y.Win1 <- x.Win1 |> fun v -> int (v * 1000m)
        y.Win2 <- x.Win2 |> fun v -> int (v * 1000m)
        y

    static member FromProtobuf(x: ProtoClasses.Test.Domain.Market) : Test.Domain.Market =
        match x.UnionCase with
        | ProtoClasses.Test.Domain.Market.UnionOneofCase.ThreeWaysMarket -> Test.Domain.Market.ThreeWaysMarket(x.ThreeWaysMarket |> ConvertTestDomain.FromProtobuf)
        | ProtoClasses.Test.Domain.Market.UnionOneofCase.TwoWaysMarket -> Test.Domain.Market.TwoWaysMarket(x.TwoWaysMarket |> ConvertTestDomain.FromProtobuf)
        | ProtoClasses.Test.Domain.Market.UnionOneofCase.HandicapMarket -> Test.Domain.Market.HandicapMarket(x.HandicapMarket |> ConvertTestDomain.FromProtobuf)
        | _ -> Test.Domain.Market.Unknown
    static member ToProtobuf(x: Test.Domain.Market) : ProtoClasses.Test.Domain.Market =
        let y = ProtoClasses.Test.Domain.Market()
        match x with
        | Test.Domain.Market.ThreeWaysMarket (p1) ->
            y.ThreeWaysMarket <- p1 |> ConvertTestDomain.ToProtobuf
        | Test.Domain.Market.TwoWaysMarket (p1) ->
            y.TwoWaysMarket <- p1 |> ConvertTestDomain.ToProtobuf
        | Test.Domain.Market.HandicapMarket (p1) ->
            y.HandicapMarket <- p1 |> ConvertTestDomain.ToProtobuf
        | Test.Domain.Market.Unknown -> ()
        y

    static member FromProtobuf(x: ProtoClasses.Test.Domain.Score) : Test.Domain.Score =
        {
            S1 = x.S1
            S2 = x.S2
        }

    static member ToProtobuf(x: Test.Domain.Score) : ProtoClasses.Test.Domain.Score =
        let y = ProtoClasses.Test.Domain.Score()
        y.S1 <- x.S1
        y.S2 <- x.S2
        y

    static member FromProtobuf(x: ProtoClasses.Test.Domain.GoalDetails) : Test.Domain.GoalDetails =
        {
            Score = x.Score |> ConvertTestDomain.FromProtobuf
            Comment = x.Comment
        }

    static member ToProtobuf(x: Test.Domain.GoalDetails) : ProtoClasses.Test.Domain.GoalDetails =
        let y = ProtoClasses.Test.Domain.GoalDetails()
        y.Score <- x.Score |> ConvertTestDomain.ToProtobuf
        y.Comment <- x.Comment
        y

    static member FromProtobuf(x: ProtoClasses.Test.Domain.MatchProtocol) : Test.Domain.MatchProtocol =
        {
            Details = x.Details |> Seq.map(ConvertTestDomain.FromProtobuf) |> List.ofSeq
        }

    static member ToProtobuf(x: Test.Domain.MatchProtocol) : ProtoClasses.Test.Domain.MatchProtocol =
        let y = ProtoClasses.Test.Domain.MatchProtocol()
        y.Details.AddRange(x.Details |> Seq.map(ConvertTestDomain.ToProtobuf))
        y

