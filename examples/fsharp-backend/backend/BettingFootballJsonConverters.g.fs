namespace Protokeep.FsharpJson

open System.Text.Json
open Protokeep

type ConvertExampleBettingFootball() =

    static member MarketFromJson(reader: byref<Utf8JsonReader>): Example.Betting.Football.Market voption =
        if FsharpJsonHelpers.moveToStartObject(&reader)then
            let mutable y = Example.Betting.Football.Market.Unknown
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Winner3Way")) then
                    let mutable _p1 = Example.Betting.Winner3Way.Default.Value
                    match ConvertExampleBetting.Winner3WayFromJson(&reader) with
                    | ValueSome v -> _p1 <- v
                    | ValueNone -> ()
                    y <- _p1 |> Example.Betting.Football.Market.Winner3Way
                else if (reader.ValueTextEquals("Handicap")) then
                    let mutable _p1 = Example.Betting.Handicap.Default.Value
                    match ConvertExampleBetting.HandicapFromJson(&reader) with
                    | ValueSome v -> _p1 <- v
                    | ValueNone -> ()
                    y <- _p1 |> Example.Betting.Football.Market.Handicap
                else if (reader.ValueTextEquals("Total")) then
                    let mutable _p1 = Example.Betting.Total.Default.Value
                    match ConvertExampleBetting.TotalFromJson(&reader) with
                    | ValueSome v -> _p1 <- v
                    | ValueNone -> ()
                    y <- _p1 |> Example.Betting.Football.Market.Total
                else if (reader.ValueTextEquals("CorrectScore")) then
                    let mutable _p1 = Example.Betting.CorrectScore.Default.Value
                    match ConvertExampleBetting.CorrectScoreFromJson(&reader) with
                    | ValueSome v -> _p1 <- v
                    | ValueNone -> ()
                    y <- _p1 |> Example.Betting.Football.Market.CorrectScore
                else reader.Skip()
            ValueSome y
        else ValueNone
    static member MarketToJson (writer:inref<Utf8JsonWriter>, x: Example.Betting.Football.Market) =
        writer.WriteStartObject()
        match x with
        | Example.Betting.Football.Market.Winner3Way (p1) ->
            writer.WritePropertyName("Winner3Way")
            ConvertExampleBetting.Winner3WayToJson(&writer, p1)
        | Example.Betting.Football.Market.Handicap (p1) ->
            writer.WritePropertyName("Handicap")
            ConvertExampleBetting.HandicapToJson(&writer, p1)
        | Example.Betting.Football.Market.Total (p1) ->
            writer.WritePropertyName("Total")
            ConvertExampleBetting.TotalToJson(&writer, p1)
        | Example.Betting.Football.Market.CorrectScore (p1) ->
            writer.WritePropertyName("CorrectScore")
            ConvertExampleBetting.CorrectScoreToJson(&writer, p1)
        | _ ->
            writer.WritePropertyName("Unknown")
            writer.WriteBooleanValue(true)
        writer.WriteEndObject()

    static member PeriodFromString =
        function
        | "PeriodHalf1" -> Example.Betting.Football.Period.Half1
        | "PeriodHalf2" -> Example.Betting.Football.Period.Half2
        | "PeriodMainTime" -> Example.Betting.Football.Period.MainTime
        | _ -> Example.Betting.Football.Period.Unknown

    static member PeriodToString =
        function
        | Example.Betting.Football.Period.Half1 -> "PeriodHalf1"
        | Example.Betting.Football.Period.Half2 -> "PeriodHalf2"
        | Example.Betting.Football.Period.MainTime -> "PeriodMainTime"
        | _ -> "Unknown"

    static member StatisticFromString =
        function
        | "StatisticGoals" -> Example.Betting.Football.Statistic.Goals
        | "StatisticYellowCards" -> Example.Betting.Football.Statistic.YellowCards
        | "StatisticCorners" -> Example.Betting.Football.Statistic.Corners
        | _ -> Example.Betting.Football.Statistic.Unknown

    static member StatisticToString =
        function
        | Example.Betting.Football.Statistic.Goals -> "StatisticGoals"
        | Example.Betting.Football.Statistic.YellowCards -> "StatisticYellowCards"
        | Example.Betting.Football.Statistic.Corners -> "StatisticCorners"
        | _ -> "Unknown"

    static member StatusFromString =
        function
        | "StatusOpen" -> Example.Betting.Football.Status.Open
        | "StatusClosed" -> Example.Betting.Football.Status.Closed
        | _ -> Example.Betting.Football.Status.Unknown

    static member StatusToString =
        function
        | Example.Betting.Football.Status.Open -> "StatusOpen"
        | Example.Betting.Football.Status.Closed -> "StatusClosed"
        | _ -> "Unknown"

    static member MarketItemToJson (writer: inref<Utf8JsonWriter>, x: Example.Betting.Football.MarketItem) =
        writer.WriteStartObject()
        writer.WritePropertyName("Statistic")
        writer.WriteStringValue(x.Statistic |> ConvertExampleBettingFootball.StatisticToString)
        writer.WritePropertyName("Period")
        writer.WriteStringValue(x.Period |> ConvertExampleBettingFootball.PeriodToString)
        writer.WritePropertyName("Market")
        ConvertExampleBettingFootball.MarketToJson(&writer, x.Market)
        writer.WritePropertyName("Status")
        writer.WriteStringValue(x.Status |> ConvertExampleBettingFootball.StatusToString)
        writer.WritePropertyName("Version")
        writer.WriteNumberValue(x.Version)
        writer.WriteEndObject()

    static member MarketItemFromJson(reader: byref<Utf8JsonReader>): Example.Betting.Football.MarketItem voption =
        let mutable vStatistic = Example.Betting.Football.Statistic.Unknown
        let mutable vPeriod = Example.Betting.Football.Period.Unknown
        let mutable vMarket = Example.Betting.Football.Market.Unknown
        let mutable vStatus = Example.Betting.Football.Status.Unknown
        let mutable vVersion = 0
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Statistic")) then
                    match FsharpJsonHelpers.readString(&reader) |> ValueOption.map ConvertExampleBettingFootball.StatisticFromString with
                    | ValueSome v -> vStatistic <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Period")) then
                    match FsharpJsonHelpers.readString(&reader) |> ValueOption.map ConvertExampleBettingFootball.PeriodFromString with
                    | ValueSome v -> vPeriod <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Market")) then
                    match ConvertExampleBettingFootball.MarketFromJson(&reader) with
                    | ValueSome v -> vMarket <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Status")) then
                    match FsharpJsonHelpers.readString(&reader) |> ValueOption.map ConvertExampleBettingFootball.StatusFromString with
                    | ValueSome v -> vStatus <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Version")) then
                    match FsharpJsonHelpers.readInt(&reader) with
                    | ValueSome v -> vVersion <- v
                    | ValueNone -> ()
                else reader.Skip()
            ValueSome {
                Statistic = vStatistic
                Period = vPeriod
                Market = vMarket
                Status = vStatus
                Version = vVersion
            }
        else ValueNone
