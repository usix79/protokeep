namespace Protokeep.FsharpProtoConverters
type ConvertBettingFootball () =
    static member FromProtobuf (x:ProtoClasses.Betting.Football.Market) : Betting.Football.Market =
        match x.UnionCase with
        | ProtoClasses.Betting.Football.Market.UnionOneofCase.Winner3Way -> Betting.Football.Market.Winner3Way(x.Winner3Way |> ConvertBetting.FromProtobuf)
        | ProtoClasses.Betting.Football.Market.UnionOneofCase.Handicap -> Betting.Football.Market.Handicap(x.Handicap |> ConvertBetting.FromProtobuf)
        | ProtoClasses.Betting.Football.Market.UnionOneofCase.Total -> Betting.Football.Market.Total(x.Total |> ConvertBetting.FromProtobuf)
        | ProtoClasses.Betting.Football.Market.UnionOneofCase.CorrectScore -> Betting.Football.Market.CorrectScore(x.CorrectScore |> ConvertBetting.FromProtobuf)
        | _ -> Betting.Football.Market.Unknown
    static member ToProtobuf (x:Betting.Football.Market) : ProtoClasses.Betting.Football.Market =
        let y = ProtoClasses.Betting.Football.Market()
        match x with
        | Betting.Football.Market.Winner3Way (p1) ->
            y.Winner3Way <- p1 |> ConvertBetting.ToProtobuf
        | Betting.Football.Market.Handicap (p1) ->
            y.Handicap <- p1 |> ConvertBetting.ToProtobuf
        | Betting.Football.Market.Total (p1) ->
            y.Total <- p1 |> ConvertBetting.ToProtobuf
        | Betting.Football.Market.CorrectScore (p1) ->
            y.CorrectScore <- p1 |> ConvertBetting.ToProtobuf
        | Betting.Football.Market.Unknown -> ()
        y
    static member FromProtobuf (x:ProtoClasses.Betting.Football.Period) : Betting.Football.Period =
        enum<Betting.Football.Period>(int x)
    static member ToProtobuf (x:Betting.Football.Period) : ProtoClasses.Betting.Football.Period =
        enum<ProtoClasses.Betting.Football.Period>(int x)
    static member FromProtobuf (x:ProtoClasses.Betting.Football.Statistic) : Betting.Football.Statistic =
        enum<Betting.Football.Statistic>(int x)
    static member ToProtobuf (x:Betting.Football.Statistic) : ProtoClasses.Betting.Football.Statistic =
        enum<ProtoClasses.Betting.Football.Statistic>(int x)
    static member FromProtobuf (x:ProtoClasses.Betting.Football.Status) : Betting.Football.Status =
        enum<Betting.Football.Status>(int x)
    static member ToProtobuf (x:Betting.Football.Status) : ProtoClasses.Betting.Football.Status =
        enum<ProtoClasses.Betting.Football.Status>(int x)
    static member FromProtobuf (x:ProtoClasses.Betting.Football.MarketItem) : Betting.Football.MarketItem =
        {
            Statistic = x.Statistic |> ConvertBettingFootball.FromProtobuf
            Period = x.Period |> ConvertBettingFootball.FromProtobuf
            Market = x.Market |> ConvertBettingFootball.FromProtobuf
            Status = x.Status |> ConvertBettingFootball.FromProtobuf
        }
    static member ToProtobuf (x:Betting.Football.MarketItem) : ProtoClasses.Betting.Football.MarketItem =
        let y = ProtoClasses.Betting.Football.MarketItem()
        y.Statistic <- x.Statistic |> ConvertBettingFootball.ToProtobuf
        y.Period <- x.Period |> ConvertBettingFootball.ToProtobuf
        y.Market <- x.Market |> ConvertBettingFootball.ToProtobuf
        y.Status <- x.Status |> ConvertBettingFootball.ToProtobuf
        y
