namespace Protokeep.FsharpJson

open System.Text.Json
open Protokeep

type ConvertBettingFootball() =

    static member MarketFromJson(reader: byref<Utf8JsonReader>): Betting.Football.Market =
        let mutable y = Betting.Football.Market.Unknown
        if FsharpJsonHelpers.moveToStartObject(&reader)then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Winner3Way")) then
                    y <- ConvertBettingFootball.Winner3WayFromJson(&reader) |> Betting.Football.Market.Winner3Way
                else if (reader.ValueTextEquals("Handicap")) then
                    y <- ConvertBettingFootball.HandicapFromJson(&reader) |> Betting.Football.Market.Handicap
                else if (reader.ValueTextEquals("Total")) then
                    y <- ConvertBettingFootball.TotalFromJson(&reader) |> Betting.Football.Market.Total
                else if (reader.ValueTextEquals("CorrectScore")) then
                    y <- ConvertBettingFootball.CorrectScoreFromJson(&reader) |> Betting.Football.Market.CorrectScore
                else reader.Skip()
        y
    static member MarketToJson (writer:inref<Utf8JsonWriter>, x: Betting.Football.Market) =
        writer.WriteStartObject()
        match x with
        | Betting.Football.Market.Winner3Way (p1) ->
            writer.WritePropertyName("Winner3Way")
            ConvertBetting.Winner3WayToJson(&writer, p1)
        | Betting.Football.Market.Handicap (p1) ->
            writer.WritePropertyName("Handicap")
            ConvertBetting.HandicapToJson(&writer, p1)
        | Betting.Football.Market.Total (p1) ->
            writer.WritePropertyName("Total")
            ConvertBetting.TotalToJson(&writer, p1)
        | Betting.Football.Market.CorrectScore (p1) ->
            writer.WritePropertyName("CorrectScore")
            ConvertBetting.CorrectScoreToJson(&writer, p1)
        | _ ->
            writer.WritePropertyName("Unknown")
            writer.WriteBooleanValue(true)
        writer.WriteEndObject()

    static member PeriodFromString =
        function
        | "PeriodHalf1" -> Betting.Football.Period.Half1
        | "PeriodHalf2" -> Betting.Football.Period.Half2
        | "PeriodMainTime" -> Betting.Football.Period.MainTime
        | _ -> Betting.Football.Period.Unknown

    static member PeriodToString =
        function
        | Betting.Football.Period.Half1 -> "PeriodHalf1"
        | Betting.Football.Period.Half2 -> "PeriodHalf2"
        | Betting.Football.Period.MainTime -> "PeriodMainTime"
        | _ -> "Unknown"

    static member StatisticFromString =
        function
        | "StatisticGoals" -> Betting.Football.Statistic.Goals
        | "StatisticYellowCards" -> Betting.Football.Statistic.YellowCards
        | "StatisticCorners" -> Betting.Football.Statistic.Corners
        | _ -> Betting.Football.Statistic.Unknown

    static member StatisticToString =
        function
        | Betting.Football.Statistic.Goals -> "StatisticGoals"
        | Betting.Football.Statistic.YellowCards -> "StatisticYellowCards"
        | Betting.Football.Statistic.Corners -> "StatisticCorners"
        | _ -> "Unknown"

    static member StatusFromString =
        function
        | "StatusOpen" -> Betting.Football.Status.Open
        | "StatusClosed" -> Betting.Football.Status.Closed
        | _ -> Betting.Football.Status.Unknown

    static member StatusToString =
        function
        | Betting.Football.Status.Open -> "StatusOpen"
        | Betting.Football.Status.Closed -> "StatusClosed"
        | _ -> "Unknown"

    static member MarketItemFromJson(reader: byref<Utf8JsonReader>): Betting.Football.MarketItem =
        let mutable vStatistic = Betting.Football.Statistic.Unknown
        let mutable vPeriod = Betting.Football.Period.Unknown
        let mutable vMarket = Betting.Football.Market.Unknown
        let mutable vStatus = Betting.Football.Status.Unknown
        let mutable vVersion = 0
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Statistic")) then
                    match FsharpJsonHelpers.readString(&reader) |> ValueOption.map ConvertBettingFootball.StatisticFromString with
                    | ValueSome v -> vStatistic <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Period")) then
                    match FsharpJsonHelpers.readString(&reader) |> ValueOption.map ConvertBettingFootball.PeriodFromString with
                    | ValueSome v -> vPeriod <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Market")) then
                    match ConvertBettingFootball.MarketFromJson(&reader) |> ValueSome with
                    | ValueSome v -> vMarket <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Status")) then
                    match FsharpJsonHelpers.readString(&reader) |> ValueOption.map ConvertBettingFootball.StatusFromString with
                    | ValueSome v -> vStatus <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Version")) then
                    match FsharpJsonHelpers.readInt(&reader) with
                    | ValueSome v -> vVersion <- v
                    | ValueNone -> ()
                else reader.Skip()
        {
            Statistic = vStatistic
            Period = vPeriod
            Market = vMarket
            Status = vStatus
            Version = vVersion
        }
    static member MarketItemToJson (writer: inref<Utf8JsonWriter>, x: Betting.Football.MarketItem) =
        writer.WriteStartObject()
        writer.WritePropertyName("Statistic")
        writer.WriteStringValue(x.Statistic |> ConvertBettingFootball.StatisticToString)
        writer.WritePropertyName("Period")
        writer.WriteStringValue(x.Period |> ConvertBettingFootball.PeriodToString)
        writer.WritePropertyName("Market")
        ConvertBettingFootball.MarketToJson(&writer, x.Market)
        writer.WritePropertyName("Status")
        writer.WriteStringValue(x.Status |> ConvertBettingFootball.StatusToString)
        writer.WritePropertyName("Version")
        writer.WriteNumberValue(x.Version)
        writer.WriteEndObject()

