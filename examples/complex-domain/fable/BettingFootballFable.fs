namespace Protokeep.FsharpFable
open Fable.SimpleJson
open Protokeep

type ConvertBettingFootball () =
    static member MarketFromJson (json: Json): Betting.Football.Market =
        let mutable y = Betting.Football.Market.Unknown
        FsharpFableHelpers.getProps json
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
    static member StatusFromString = function
        | "StatusOpen" -> Betting.Football.Status.Open
        | "StatusClosed" -> Betting.Football.Status.Closed
        | _ -> Betting.Football.Status.Unknown
    static member StatusToString = function
        | Betting.Football.Status.Open -> "StatusOpen"
        | Betting.Football.Status.Closed -> "StatusClosed"
        | _ -> "Unknown"
    static member MarketItemFromJson (json: Json): Betting.Football.MarketItem =
        let mutable vStatistic = Betting.Football.Statistic.Unknown
        let mutable vPeriod = Betting.Football.Period.Unknown
        let mutable vMarket = Betting.Football.Market.Unknown
        let mutable vStatus = Betting.Football.Status.Unknown
        let mutable vVersion = 0
        FsharpFableHelpers.getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Statistic" -> pair.Value |> FsharpFableHelpers.ifString (fun v -> vStatistic <- v |> ConvertBettingFootball.StatisticFromString)
            | "Period" -> pair.Value |> FsharpFableHelpers.ifString (fun v -> vPeriod <- v |> ConvertBettingFootball.PeriodFromString)
            | "Market" -> pair.Value |> (fun v -> vMarket <- v |> ConvertBettingFootball.MarketFromJson)
            | "Status" -> pair.Value |> FsharpFableHelpers.ifString (fun v -> vStatus <- v |> ConvertBettingFootball.StatusFromString)
            | "Version" -> pair.Value |> FsharpFableHelpers.ifNumber (fun v -> vVersion <- v |> unbox)
            | _ -> () )
        {
            Statistic = vStatistic
            Period = vPeriod
            Market = vMarket
            Status = vStatus
            Version = vVersion
        }
    static member MarketItemToJson (x: Betting.Football.MarketItem) =
        [
           "Statistic", JString (x.Statistic |> ConvertBettingFootball.StatisticToString)
           "Period", JString (x.Period |> ConvertBettingFootball.PeriodToString)
           "Market", (x.Market |> ConvertBettingFootball.MarketToJson)
           "Status", JString (x.Status |> ConvertBettingFootball.StatusToString)
           "Version", JNumber (unbox x.Version)
        ] |> Map.ofList |> JObject
