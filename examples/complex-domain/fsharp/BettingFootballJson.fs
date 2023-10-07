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
                    if reader.Read() && reader.TokenType = JsonTokenType.StartObject
                    then y <- ConvertBetting.Winner3WayFromJson(&reader) |> Betting.Football.Market.Winner3Way
                    else reader.Skip()
                else if (reader.ValueTextEquals("Handicap")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.StartObject
                    then y <- ConvertBetting.HandicapFromJson(&reader) |> Betting.Football.Market.Handicap
                    else reader.Skip()
                else if (reader.ValueTextEquals("Total")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.StartObject
                    then y <- ConvertBetting.TotalFromJson(&reader) |> Betting.Football.Market.Total
                    else reader.Skip()
                else if (reader.ValueTextEquals("CorrectScore")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.StartObject
                    then y <- ConvertBetting.CorrectScoreFromJson(&reader) |> Betting.Football.Market.CorrectScore
                    else reader.Skip()
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
                    if reader.Read() && reader.TokenType = JsonTokenType.String
                    then vStatistic <- reader.GetString() |> ConvertBettingFootball.StatisticFromString
                    else reader.Skip()
                else if (reader.ValueTextEquals("Period")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.String
                    then vPeriod <- reader.GetString() |> ConvertBettingFootball.PeriodFromString
                    else reader.Skip()
                else if (reader.ValueTextEquals("Market")) then
                    vMarket <- ConvertBettingFootball.MarketFromJson(&reader)
                else if (reader.ValueTextEquals("Status")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.String
                    then vStatus <- reader.GetString() |> ConvertBettingFootball.StatusFromString
                    else reader.Skip()
                else if (reader.ValueTextEquals("Version")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.Number
                    then vVersion <- reader.GetInt32()
                    else reader.Skip()
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

