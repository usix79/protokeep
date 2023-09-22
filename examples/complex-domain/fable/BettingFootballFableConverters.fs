namespace Protokeep.FableConverters
open Fable.SimpleJson
open Protokeep.FableConverterHelpers
type ConvertBettingFootball () =
    static member MarketFromJson (json: Json): Betting.Football.Market =
        let mutable y = Betting.Football.Market.Unknown
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Winner3Way" -> pair.Value |> (fun v -> y <- v |> ConvertBetting.Winner3WayFromJson |> Betting.Football.Market.Winner3Way)
            | "Handicap" -> pair.Value |> (fun v -> y <- v |> ConvertBetting.HandicapFromJson |> Betting.Football.Market.Handicap)
            | "Total" -> pair.Value |> (fun v -> y <- v |> ConvertBetting.TotalFromJson |> Betting.Football.Market.Total)
            | "CorrectScore" -> pair.Value |> (fun v -> y <- v |> ConvertBetting.CorrectScoreFromJson |> Betting.Football.Market.CorrectScore)
            | _ -> () )
        y
    static member MarketToJson (x:Betting.Football.Market) =
        match x with
        | Betting.Football.Market.Winner3Way (p1) -> "Winner3Way", (p1 |> ConvertBetting.Winner3WayToJson)
        | Betting.Football.Market.Handicap (p1) -> "Handicap", (p1 |> ConvertBetting.HandicapToJson)
        | Betting.Football.Market.Total (p1) -> "Total", (p1 |> ConvertBetting.TotalToJson)
        | Betting.Football.Market.CorrectScore (p1) -> "CorrectScore", (p1 |> ConvertBetting.CorrectScoreToJson)
        | _ -> "Unknown", JBool (true)
        |> List.singleton |> Map.ofList |> JObject
    static member DefaultPeriod =
        lazy Betting.Football.Period.Unknown
    static member PeriodFromString = function
        | "PeriodHalf1" -> Betting.Football.Period.Half1
        | "PeriodHalf2" -> Betting.Football.Period.Half2
        | "PeriodMainTime" -> Betting.Football.Period.MainTime
        | _ -> Betting.Football.Period.Unknown
    static member PeriodToString = function
        | Betting.Football.Period.Half1 -> "PeriodHalf1"
        | Betting.Football.Period.Half2 -> "PeriodHalf2"
        | Betting.Football.Period.MainTime -> "PeriodMainTime"
        | _ -> "Unknown"
    static member DefaultStatistic =
        lazy Betting.Football.Statistic.Unknown
    static member StatisticFromString = function
        | "StatisticGoals" -> Betting.Football.Statistic.Goals
        | "StatisticYellowCards" -> Betting.Football.Statistic.YellowCards
        | "StatisticCorners" -> Betting.Football.Statistic.Corners
        | _ -> Betting.Football.Statistic.Unknown
    static member StatisticToString = function
        | Betting.Football.Statistic.Goals -> "StatisticGoals"
        | Betting.Football.Statistic.YellowCards -> "StatisticYellowCards"
        | Betting.Football.Statistic.Corners -> "StatisticCorners"
        | _ -> "Unknown"
    static member DefaultStatus =
        lazy Betting.Football.Status.Unknown
    static member StatusFromString = function
        | "StatusOpen" -> Betting.Football.Status.Open
        | "StatusClosed" -> Betting.Football.Status.Closed
        | _ -> Betting.Football.Status.Unknown
    static member StatusToString = function
        | Betting.Football.Status.Open -> "StatusOpen"
        | Betting.Football.Status.Closed -> "StatusClosed"
        | _ -> "Unknown"
    static member DefaultMarketItem: Lazy<Betting.Football.MarketItem> =
        lazy {
            Statistic = ConvertBettingFootball.DefaultStatistic.Value
            Period = ConvertBettingFootball.DefaultPeriod.Value
            Market = Betting.Football.Market.Unknown
            Status = ConvertBettingFootball.DefaultStatus.Value
        }
    static member MarketItemFromJson (json: Json): Betting.Football.MarketItem =
        let mutable vStatistic = ConvertBettingFootball.DefaultStatistic.Value
        let mutable vPeriod = ConvertBettingFootball.DefaultPeriod.Value
        let mutable vMarket = Betting.Football.Market.Unknown
        let mutable vStatus = ConvertBettingFootball.DefaultStatus.Value
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Statistic" -> pair.Value |> ifString (fun v -> vStatistic <- v |> ConvertBettingFootball.StatisticFromString)
            | "Period" -> pair.Value |> ifString (fun v -> vPeriod <- v |> ConvertBettingFootball.PeriodFromString)
            | "Market" -> pair.Value |> (fun v -> vMarket <- v |> ConvertBettingFootball.MarketFromJson)
            | "Status" -> pair.Value |> ifString (fun v -> vStatus <- v |> ConvertBettingFootball.StatusFromString)
            | _ -> () )
        {
            Statistic = vStatistic
            Period = vPeriod
            Market = vMarket
            Status = vStatus
        }
    static member MarketItemToJson (x: Betting.Football.MarketItem) =
        [
           "Statistic", JString (x.Statistic |> ConvertBettingFootball.StatisticToString)
           "Period", JString (x.Period |> ConvertBettingFootball.PeriodToString)
           "Market", (x.Market |> ConvertBettingFootball.MarketToJson)
           "Status", JString (x.Status |> ConvertBettingFootball.StatusToString)
        ] |> Map.ofList |> JObject
