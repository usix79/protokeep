namespace Protokeep.FsharpJson

open System.Text.Json
open Protokeep

type ConvertBetting() =

    static member OutcomeResultFromString =
        function
        | "OutcomeResultWin" -> Betting.OutcomeResult.Win
        | "OutcomeResultLose" -> Betting.OutcomeResult.Lose
        | "OutcomeResultVoid" -> Betting.OutcomeResult.Void
        | "OutcomeResultCanceled" -> Betting.OutcomeResult.Canceled
        | _ -> Betting.OutcomeResult.Unknown

    static member OutcomeResultToString =
        function
        | Betting.OutcomeResult.Win -> "OutcomeResultWin"
        | Betting.OutcomeResult.Lose -> "OutcomeResultLose"
        | Betting.OutcomeResult.Void -> "OutcomeResultVoid"
        | Betting.OutcomeResult.Canceled -> "OutcomeResultCanceled"
        | _ -> "Unknown"

    static member OutcomeFromJson(reader: byref<Utf8JsonReader>): Betting.Outcome =
        let mutable y = Betting.Outcome.Unknown
        if FsharpJsonHelpers.moveToStartObject(&reader)then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Empty")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.True
                    then y <- Betting.Outcome.Empty
                    else reader.Skip()
                else if (reader.ValueTextEquals("Priced")) then
                    match FsharpJsonHelpers.readMoney(&reader) with
                    | ValueSome v -> y <- v |> Betting.Outcome.Priced
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("PricedWithProb")) then
                    y <- ConvertBetting.OutcomeCasePricedWithProbFromJson(&reader)
                else if (reader.ValueTextEquals("Resulted")) then
                    y <- ConvertBetting.OutcomeResultFromJson(&reader) |> Betting.Outcome.Resulted
                else reader.Skip()
        y
    static member OutcomeToJson (writer:inref<Utf8JsonWriter>, x: Betting.Outcome) =
        writer.WriteStartObject()
        match x with
        | Betting.Outcome.Empty ->
            writer.WritePropertyName("Empty")
            writer.WriteBooleanValue(true)
        | Betting.Outcome.Priced (price) ->
            writer.WritePropertyName("Priced")
            writer.WriteNumberValue(price)
        | Betting.Outcome.PricedWithProb (price,prob) ->
            writer.WritePropertyName("PricedWithProb")
            ConvertBetting.OutcomeCasePricedWithProbToJson(&writer,price,prob)
        | Betting.Outcome.Resulted (result) ->
            writer.WritePropertyName("Resulted")
            writer.WriteStringValue(result |> ConvertBetting.OutcomeResultToString)
        | _ ->
            writer.WritePropertyName("Unknown")
            writer.WriteBooleanValue(true)
        writer.WriteEndObject()
    static member OutcomeCasePricedWithProbFromJson(reader: byref<Utf8JsonReader>) =
        let mutable price = 0m
        let mutable prob = 0.f
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Price")) then
                    match FsharpJsonHelpers.readMoney(&reader) with
                    | ValueSome v -> price <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Prob")) then
                    match FsharpJsonHelpers.readSingle(&reader) with
                    | ValueSome v -> prob <- v
                    | ValueNone -> ()
                else reader.Skip()
        Betting.Outcome.PricedWithProb (price,prob)
    static member OutcomeCasePricedWithProbToJson (writer: inref<Utf8JsonWriter>,price,prob) =
        writer.WriteStartObject()
        writer.WritePropertyName("Price")
        writer.WriteNumberValue(price)
        writer.WritePropertyName("Prob")
        writer.WriteNumberValue(prob)
        writer.WriteEndObject()

    static member Winner3WayFromJson(reader: byref<Utf8JsonReader>): Betting.Winner3Way =
        let mutable vWin1 = Betting.Outcome.Unknown
        let mutable vDraw = Betting.Outcome.Unknown
        let mutable vWin2 = Betting.Outcome.Unknown
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Win1")) then
                    match ConvertBetting.OutcomeFromJson(&reader) |> ValueSome with
                    | ValueSome v -> vWin1 <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Draw")) then
                    match ConvertBetting.OutcomeFromJson(&reader) |> ValueSome with
                    | ValueSome v -> vDraw <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Win2")) then
                    match ConvertBetting.OutcomeFromJson(&reader) |> ValueSome with
                    | ValueSome v -> vWin2 <- v
                    | ValueNone -> ()
                else reader.Skip()
        {
            Win1 = vWin1
            Draw = vDraw
            Win2 = vWin2
        }
    static member Winner3WayToJson (writer: inref<Utf8JsonWriter>, x: Betting.Winner3Way) =
        writer.WriteStartObject()
        writer.WritePropertyName("Win1")
        ConvertBetting.OutcomeToJson(&writer, x.Win1)
        writer.WritePropertyName("Draw")
        ConvertBetting.OutcomeToJson(&writer, x.Draw)
        writer.WritePropertyName("Win2")
        ConvertBetting.OutcomeToJson(&writer, x.Win2)
        writer.WriteEndObject()

    static member HandicapFromJson(reader: byref<Utf8JsonReader>): Betting.Handicap =
        let mutable vValue = 0m
        let mutable vWin1 = Betting.Outcome.Unknown
        let mutable vWin2 = Betting.Outcome.Unknown
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Value")) then
                    match FsharpJsonHelpers.readMoney(&reader) with
                    | ValueSome v -> vValue <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Win1")) then
                    match ConvertBetting.OutcomeFromJson(&reader) |> ValueSome with
                    | ValueSome v -> vWin1 <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Win2")) then
                    match ConvertBetting.OutcomeFromJson(&reader) |> ValueSome with
                    | ValueSome v -> vWin2 <- v
                    | ValueNone -> ()
                else reader.Skip()
        {
            Value = vValue
            Win1 = vWin1
            Win2 = vWin2
        }
    static member HandicapToJson (writer: inref<Utf8JsonWriter>, x: Betting.Handicap) =
        writer.WriteStartObject()
        writer.WritePropertyName("Value")
        writer.WriteNumberValue(x.Value)
        writer.WritePropertyName("Win1")
        ConvertBetting.OutcomeToJson(&writer, x.Win1)
        writer.WritePropertyName("Win2")
        ConvertBetting.OutcomeToJson(&writer, x.Win2)
        writer.WriteEndObject()

    static member TotalFromJson(reader: byref<Utf8JsonReader>): Betting.Total =
        let mutable vValue = 0m
        let mutable vOver = Betting.Outcome.Unknown
        let mutable vUnder = Betting.Outcome.Unknown
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Value")) then
                    match FsharpJsonHelpers.readMoney(&reader) with
                    | ValueSome v -> vValue <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Over")) then
                    match ConvertBetting.OutcomeFromJson(&reader) |> ValueSome with
                    | ValueSome v -> vOver <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Under")) then
                    match ConvertBetting.OutcomeFromJson(&reader) |> ValueSome with
                    | ValueSome v -> vUnder <- v
                    | ValueNone -> ()
                else reader.Skip()
        {
            Value = vValue
            Over = vOver
            Under = vUnder
        }
    static member TotalToJson (writer: inref<Utf8JsonWriter>, x: Betting.Total) =
        writer.WriteStartObject()
        writer.WritePropertyName("Value")
        writer.WriteNumberValue(x.Value)
        writer.WritePropertyName("Over")
        ConvertBetting.OutcomeToJson(&writer, x.Over)
        writer.WritePropertyName("Under")
        ConvertBetting.OutcomeToJson(&writer, x.Under)
        writer.WriteEndObject()

    static member ScoreFromJson(reader: byref<Utf8JsonReader>): Betting.Score =
        let mutable vS1 = 0
        let mutable vS2 = 0
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("S1")) then
                    match FsharpJsonHelpers.readInt(&reader) with
                    | ValueSome v -> vS1 <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("S2")) then
                    match FsharpJsonHelpers.readInt(&reader) with
                    | ValueSome v -> vS2 <- v
                    | ValueNone -> ()
                else reader.Skip()
        {
            S1 = vS1
            S2 = vS2
        }
    static member ScoreToJson (writer: inref<Utf8JsonWriter>, x: Betting.Score) =
        writer.WriteStartObject()
        writer.WritePropertyName("S1")
        writer.WriteNumberValue(x.S1)
        writer.WritePropertyName("S2")
        writer.WriteNumberValue(x.S2)
        writer.WriteEndObject()

    static member ScoreOutcomeFromJson(reader: byref<Utf8JsonReader>): Betting.ScoreOutcome =
        let mutable vScore = ConvertBetting.DefaultScore.Value
        let mutable vOutcome = Betting.Outcome.Unknown
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Score")) then
                    match ConvertBetting.ScoreFromJson(&reader) |> ValueSome with
                    | ValueSome v -> vScore <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Outcome")) then
                    match ConvertBetting.OutcomeFromJson(&reader) |> ValueSome with
                    | ValueSome v -> vOutcome <- v
                    | ValueNone -> ()
                else reader.Skip()
        {
            Score = vScore
            Outcome = vOutcome
        }
    static member ScoreOutcomeToJson (writer: inref<Utf8JsonWriter>, x: Betting.ScoreOutcome) =
        writer.WriteStartObject()
        writer.WritePropertyName("Score")
        ConvertBetting.ScoreToJson(&writer, x.Score)
        writer.WritePropertyName("Outcome")
        ConvertBetting.OutcomeToJson(&writer, x.Outcome)
        writer.WriteEndObject()

    static member CorrectScoreFromJson(reader: byref<Utf8JsonReader>): Betting.CorrectScore =
        let mutable vScores = ResizeArray()
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Scores")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.StartArray then
                        while reader.Read() && reader.TokenType <> JsonTokenType.EndArray do
                            match ConvertBetting.ScoreOutcomeFromJson(&reader) |> ValueSome with
                            | ValueSome v -> vScores.Add(v)
                            | ValueNone -> ()
                    else reader.Skip()
                else reader.Skip()
        {
            Scores = vScores |> List.ofSeq
        }
    static member CorrectScoreToJson (writer: inref<Utf8JsonWriter>, x: Betting.CorrectScore) =
        writer.WriteStartObject()
        writer.WritePropertyName("Scores")
        writer.WriteStartArray(); (for v in x.Scores do ConvertBetting.ScoreOutcomeToJson(&writer, v)); writer.WriteEndArray()
        writer.WriteEndObject()

