namespace Protokeep.FsharpProto

type ConvertExampleBettingFootball() =

    static member FromProtobuf(x: ProtoClasses.Example.Betting.Football.Market) : Example.Betting.Football.Market =
        match x.UnionCase with
        | ProtoClasses.Example.Betting.Football.Market.UnionOneofCase.Winner3Way -> Example.Betting.Football.Market.Winner3Way(x.Winner3Way |> ConvertExampleBetting.FromProtobuf)
        | ProtoClasses.Example.Betting.Football.Market.UnionOneofCase.Handicap -> Example.Betting.Football.Market.Handicap(x.Handicap |> ConvertExampleBetting.FromProtobuf)
        | ProtoClasses.Example.Betting.Football.Market.UnionOneofCase.Total -> Example.Betting.Football.Market.Total(x.Total |> ConvertExampleBetting.FromProtobuf)
        | ProtoClasses.Example.Betting.Football.Market.UnionOneofCase.CorrectScore -> Example.Betting.Football.Market.CorrectScore(x.CorrectScore |> ConvertExampleBetting.FromProtobuf)
        | _ -> Example.Betting.Football.Market.Unknown
    static member ToProtobuf(x: Example.Betting.Football.Market) : ProtoClasses.Example.Betting.Football.Market =
        let y = ProtoClasses.Example.Betting.Football.Market()
        match x with
        | Example.Betting.Football.Market.Winner3Way (p1) ->
            y.Winner3Way <- p1 |> ConvertExampleBetting.ToProtobuf
        | Example.Betting.Football.Market.Handicap (p1) ->
            y.Handicap <- p1 |> ConvertExampleBetting.ToProtobuf
        | Example.Betting.Football.Market.Total (p1) ->
            y.Total <- p1 |> ConvertExampleBetting.ToProtobuf
        | Example.Betting.Football.Market.CorrectScore (p1) ->
            y.CorrectScore <- p1 |> ConvertExampleBetting.ToProtobuf
        | Example.Betting.Football.Market.Unknown -> ()
        y

    static member FromProtobuf(x: ProtoClasses.Example.Betting.Football.Period) : Example.Betting.Football.Period =
        enum<Example.Betting.Football.Period> (int x)

    static member ToProtobuf(x: Example.Betting.Football.Period) : ProtoClasses.Example.Betting.Football.Period =
        enum<ProtoClasses.Example.Betting.Football.Period> (int x)

    static member FromProtobuf(x: ProtoClasses.Example.Betting.Football.Statistic) : Example.Betting.Football.Statistic =
        enum<Example.Betting.Football.Statistic> (int x)

    static member ToProtobuf(x: Example.Betting.Football.Statistic) : ProtoClasses.Example.Betting.Football.Statistic =
        enum<ProtoClasses.Example.Betting.Football.Statistic> (int x)

    static member FromProtobuf(x: ProtoClasses.Example.Betting.Football.Status) : Example.Betting.Football.Status =
        enum<Example.Betting.Football.Status> (int x)

    static member ToProtobuf(x: Example.Betting.Football.Status) : ProtoClasses.Example.Betting.Football.Status =
        enum<ProtoClasses.Example.Betting.Football.Status> (int x)

    static member FromProtobuf(x: ProtoClasses.Example.Betting.Football.MarketItem) : Example.Betting.Football.MarketItem =
        {
            Statistic = x.Statistic |> ConvertExampleBettingFootball.FromProtobuf
            Period = x.Period |> ConvertExampleBettingFootball.FromProtobuf
            Market = x.Market |> ConvertExampleBettingFootball.FromProtobuf
            Status = x.Status |> ConvertExampleBettingFootball.FromProtobuf
            Version = x.Version
        }

    static member ToProtobuf(x: Example.Betting.Football.MarketItem) : ProtoClasses.Example.Betting.Football.MarketItem =
        let y = ProtoClasses.Example.Betting.Football.MarketItem()
        y.Statistic <- x.Statistic |> ConvertExampleBettingFootball.ToProtobuf
        y.Period <- x.Period |> ConvertExampleBettingFootball.ToProtobuf
        y.Market <- x.Market |> ConvertExampleBettingFootball.ToProtobuf
        y.Status <- x.Status |> ConvertExampleBettingFootball.ToProtobuf
        y.Version <- x.Version
        y

