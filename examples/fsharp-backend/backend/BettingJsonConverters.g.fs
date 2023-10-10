namespace Protokeep.FsharpJson

open System.Text.Json
open Protokeep

type ConvertExampleBetting() =

    static member OutcomeResultFromString =
        function
        | "OutcomeResultWin" -> Example.Betting.OutcomeResult.Win
        | "OutcomeResultLose" -> Example.Betting.OutcomeResult.Lose
        | "OutcomeResultVoid" -> Example.Betting.OutcomeResult.Void
        | "OutcomeResultCanceled" -> Example.Betting.OutcomeResult.Canceled
        | _ -> Example.Betting.OutcomeResult.Unknown

    static member OutcomeResultToString =
        function
        | Example.Betting.OutcomeResult.Win -> "OutcomeResultWin"
        | Example.Betting.OutcomeResult.Lose -> "OutcomeResultLose"
        | Example.Betting.OutcomeResult.Void -> "OutcomeResultVoid"
        | Example.Betting.OutcomeResult.Canceled -> "OutcomeResultCanceled"
        | _ -> "Unknown"

    static member OutcomeFromJson(reader: byref<Utf8JsonReader>): Example.Betting.Outcome voption =
        if FsharpJsonHelpers.moveToStartObject(&reader)then
            let mutable y = Example.Betting.Outcome.Unknown
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Empty")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.True
                    then y <- Example.Betting.Outcome.Empty
                    else reader.Skip()
                else if (reader.ValueTextEquals("Priced")) then
                    let mutable _price = 0m
                    match FsharpJsonHelpers.readMoney(&reader, 3) with
                    | ValueSome v -> _price <- v
                    | ValueNone -> ()
                    y <- _price |> Example.Betting.Outcome.Priced
                else if (reader.ValueTextEquals("PricedWithProb")) then
                    y <- ConvertExampleBetting.OutcomeCasePricedWithProbFromJson(&reader)
                else if (reader.ValueTextEquals("Resulted")) then
                    let mutable _result = Example.Betting.OutcomeResult.Unknown
                    match FsharpJsonHelpers.readString(&reader) |> ValueOption.map ConvertExampleBetting.OutcomeResultFromString with
                    | ValueSome v -> _result <- v
                    | ValueNone -> ()
                    y <- _result |> Example.Betting.Outcome.Resulted
                else reader.Skip()
            ValueSome y
        else ValueNone
    static member OutcomeToJson (writer:inref<Utf8JsonWriter>, x: Example.Betting.Outcome) =
        writer.WriteStartObject()
        match x with
        | Example.Betting.Outcome.Empty ->
            writer.WritePropertyName("Empty")
            writer.WriteBooleanValue(true)
        | Example.Betting.Outcome.Priced (price) ->
            writer.WritePropertyName("Priced")
            writer.WriteNumberValue(price)
        | Example.Betting.Outcome.PricedWithProb (price,prob) ->
            writer.WritePropertyName("PricedWithProb")
            ConvertExampleBetting.OutcomeCasePricedWithProbToJson(&writer,price,prob)
        | Example.Betting.Outcome.Resulted (result) ->
            writer.WritePropertyName("Resulted")
            writer.WriteStringValue(result |> ConvertExampleBetting.OutcomeResultToString)
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
                    match FsharpJsonHelpers.readMoney(&reader, 3) with
                    | ValueSome v -> price <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Prob")) then
                    match FsharpJsonHelpers.readSingle(&reader) with
                    | ValueSome v -> prob <- v
                    | ValueNone -> ()
                else reader.Skip()
        Example.Betting.Outcome.PricedWithProb (price,prob)
    static member OutcomeCasePricedWithProbToJson (writer: inref<Utf8JsonWriter>,price,prob) =
        writer.WriteStartObject()
        writer.WritePropertyName("Price")
        writer.WriteNumberValue(price)
        writer.WritePropertyName("Prob")
        writer.WriteNumberValue(prob)
        writer.WriteEndObject()

    static member Winner3WayToJson (writer: inref<Utf8JsonWriter>, x: Example.Betting.Winner3Way) =
        writer.WriteStartObject()
        writer.WritePropertyName("Win1")
        ConvertExampleBetting.OutcomeToJson(&writer, x.Win1)
        writer.WritePropertyName("Draw")
        ConvertExampleBetting.OutcomeToJson(&writer, x.Draw)
        writer.WritePropertyName("Win2")
        ConvertExampleBetting.OutcomeToJson(&writer, x.Win2)
        writer.WriteEndObject()

    static member Winner3WayFromJson(reader: byref<Utf8JsonReader>): Example.Betting.Winner3Way voption =
        let mutable vWin1 = Example.Betting.Outcome.Unknown
        let mutable vDraw = Example.Betting.Outcome.Unknown
        let mutable vWin2 = Example.Betting.Outcome.Unknown
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Win1")) then
                    match ConvertExampleBetting.OutcomeFromJson(&reader) with
                    | ValueSome v -> vWin1 <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Draw")) then
                    match ConvertExampleBetting.OutcomeFromJson(&reader) with
                    | ValueSome v -> vDraw <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Win2")) then
                    match ConvertExampleBetting.OutcomeFromJson(&reader) with
                    | ValueSome v -> vWin2 <- v
                    | ValueNone -> ()
                else reader.Skip()
            ValueSome {
                Win1 = vWin1
                Draw = vDraw
                Win2 = vWin2
            }
        else ValueNone
    static member HandicapToJson (writer: inref<Utf8JsonWriter>, x: Example.Betting.Handicap) =
        writer.WriteStartObject()
        writer.WritePropertyName("Value")
        writer.WriteNumberValue(x.Value)
        writer.WritePropertyName("Win1")
        ConvertExampleBetting.OutcomeToJson(&writer, x.Win1)
        writer.WritePropertyName("Win2")
        ConvertExampleBetting.OutcomeToJson(&writer, x.Win2)
        writer.WriteEndObject()

    static member HandicapFromJson(reader: byref<Utf8JsonReader>): Example.Betting.Handicap voption =
        let mutable vValue = 0m
        let mutable vWin1 = Example.Betting.Outcome.Unknown
        let mutable vWin2 = Example.Betting.Outcome.Unknown
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Value")) then
                    match FsharpJsonHelpers.readMoney(&reader, 2) with
                    | ValueSome v -> vValue <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Win1")) then
                    match ConvertExampleBetting.OutcomeFromJson(&reader) with
                    | ValueSome v -> vWin1 <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Win2")) then
                    match ConvertExampleBetting.OutcomeFromJson(&reader) with
                    | ValueSome v -> vWin2 <- v
                    | ValueNone -> ()
                else reader.Skip()
            ValueSome {
                Value = vValue
                Win1 = vWin1
                Win2 = vWin2
            }
        else ValueNone
    static member TotalToJson (writer: inref<Utf8JsonWriter>, x: Example.Betting.Total) =
        writer.WriteStartObject()
        writer.WritePropertyName("Value")
        writer.WriteNumberValue(x.Value)
        writer.WritePropertyName("Over")
        ConvertExampleBetting.OutcomeToJson(&writer, x.Over)
        writer.WritePropertyName("Under")
        ConvertExampleBetting.OutcomeToJson(&writer, x.Under)
        writer.WriteEndObject()

    static member TotalFromJson(reader: byref<Utf8JsonReader>): Example.Betting.Total voption =
        let mutable vValue = 0m
        let mutable vOver = Example.Betting.Outcome.Unknown
        let mutable vUnder = Example.Betting.Outcome.Unknown
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Value")) then
                    match FsharpJsonHelpers.readMoney(&reader, 2) with
                    | ValueSome v -> vValue <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Over")) then
                    match ConvertExampleBetting.OutcomeFromJson(&reader) with
                    | ValueSome v -> vOver <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Under")) then
                    match ConvertExampleBetting.OutcomeFromJson(&reader) with
                    | ValueSome v -> vUnder <- v
                    | ValueNone -> ()
                else reader.Skip()
            ValueSome {
                Value = vValue
                Over = vOver
                Under = vUnder
            }
        else ValueNone
    static member ScoreToJson (writer: inref<Utf8JsonWriter>, x: Example.Betting.Score) =
        writer.WriteStartObject()
        writer.WritePropertyName("S1")
        writer.WriteNumberValue(x.S1)
        writer.WritePropertyName("S2")
        writer.WriteNumberValue(x.S2)
        writer.WriteEndObject()

    static member ScoreFromJson(reader: byref<Utf8JsonReader>): Example.Betting.Score voption =
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
            ValueSome {
                S1 = vS1
                S2 = vS2
            }
        else ValueNone
    static member ScoreOutcomeToJson (writer: inref<Utf8JsonWriter>, x: Example.Betting.ScoreOutcome) =
        writer.WriteStartObject()
        writer.WritePropertyName("Score")
        ConvertExampleBetting.ScoreToJson(&writer, x.Score)
        writer.WritePropertyName("Outcome")
        ConvertExampleBetting.OutcomeToJson(&writer, x.Outcome)
        writer.WriteEndObject()

    static member ScoreOutcomeFromJson(reader: byref<Utf8JsonReader>): Example.Betting.ScoreOutcome voption =
        let mutable vScore = Example.Betting.Score.Default.Value
        let mutable vOutcome = Example.Betting.Outcome.Unknown
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Score")) then
                    match ConvertExampleBetting.ScoreFromJson(&reader) with
                    | ValueSome v -> vScore <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Outcome")) then
                    match ConvertExampleBetting.OutcomeFromJson(&reader) with
                    | ValueSome v -> vOutcome <- v
                    | ValueNone -> ()
                else reader.Skip()
            ValueSome {
                Score = vScore
                Outcome = vOutcome
            }
        else ValueNone
    static member CorrectScoreToJson (writer: inref<Utf8JsonWriter>, x: Example.Betting.CorrectScore) =
        writer.WriteStartObject()
        writer.WritePropertyName("Scores")
        writer.WriteStartArray()
        for v in x.Scores do
            ConvertExampleBetting.ScoreOutcomeToJson(&writer, v)
        writer.WriteEndArray()
        writer.WriteEndObject()

    static member CorrectScoreFromJson(reader: byref<Utf8JsonReader>): Example.Betting.CorrectScore voption =
        let mutable vScores = ResizeArray()
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Scores")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.StartArray then
                        while reader.TokenType <> JsonTokenType.EndArray do
                            match ConvertExampleBetting.ScoreOutcomeFromJson(&reader) with
                            | ValueSome v -> vScores.Add(v)
                            | ValueNone -> ()
                    else reader.Skip()
                else reader.Skip()
            ValueSome {
                Scores = vScores |> List.ofSeq
            }
        else ValueNone
