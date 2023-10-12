namespace Test.Converters

open System.Text.Json
open Protokeep

type ConvertTestDomain() =

    static member ThreeWaysMarketToJson (writer: inref<Utf8JsonWriter>, x: Test.Domain.ThreeWaysMarket) =
        writer.WriteStartObject()
        writer.WritePropertyName("Win1")
        writer.WriteNumberValue(x.Win1)
        writer.WritePropertyName("Draw")
        writer.WriteNumberValue(x.Draw)
        writer.WritePropertyName("Win2")
        writer.WriteNumberValue(x.Win2)
        writer.WriteEndObject()

    static member ThreeWaysMarketFromJson(reader: byref<Utf8JsonReader>): Test.Domain.ThreeWaysMarket voption =
        let mutable vWin1 = 0m
        let mutable vDraw = 0m
        let mutable vWin2 = 0m
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Win1")) then
                    match FsharpJsonHelpers.readMoney(&reader, 3) with
                    | ValueSome v -> vWin1 <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Draw")) then
                    match FsharpJsonHelpers.readMoney(&reader, 3) with
                    | ValueSome v -> vDraw <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Win2")) then
                    match FsharpJsonHelpers.readMoney(&reader, 3) with
                    | ValueSome v -> vWin2 <- v
                    | ValueNone -> ()
                else reader.Skip()
            ValueSome {
                Win1 = vWin1
                Draw = vDraw
                Win2 = vWin2
            }
        else ValueNone
    static member TwoWaysMarketToJson (writer: inref<Utf8JsonWriter>, x: Test.Domain.TwoWaysMarket) =
        writer.WriteStartObject()
        writer.WritePropertyName("Win1")
        writer.WriteNumberValue(x.Win1)
        writer.WritePropertyName("Win2")
        writer.WriteNumberValue(x.Win2)
        writer.WriteEndObject()

    static member TwoWaysMarketFromJson(reader: byref<Utf8JsonReader>): Test.Domain.TwoWaysMarket voption =
        let mutable vWin1 = 0m
        let mutable vWin2 = 0m
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Win1")) then
                    match FsharpJsonHelpers.readMoney(&reader, 3) with
                    | ValueSome v -> vWin1 <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Win2")) then
                    match FsharpJsonHelpers.readMoney(&reader, 3) with
                    | ValueSome v -> vWin2 <- v
                    | ValueNone -> ()
                else reader.Skip()
            ValueSome {
                Win1 = vWin1
                Win2 = vWin2
            }
        else ValueNone
    static member HandicapMarketToJson (writer: inref<Utf8JsonWriter>, x: Test.Domain.HandicapMarket) =
        writer.WriteStartObject()
        writer.WritePropertyName("Value")
        writer.WriteNumberValue(x.Value)
        writer.WritePropertyName("Win1")
        writer.WriteNumberValue(x.Win1)
        writer.WritePropertyName("Win2")
        writer.WriteNumberValue(x.Win2)
        writer.WriteEndObject()

    static member HandicapMarketFromJson(reader: byref<Utf8JsonReader>): Test.Domain.HandicapMarket voption =
        let mutable vValue = 0m
        let mutable vWin1 = 0m
        let mutable vWin2 = 0m
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Value")) then
                    match FsharpJsonHelpers.readMoney(&reader, 2) with
                    | ValueSome v -> vValue <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Win1")) then
                    match FsharpJsonHelpers.readMoney(&reader, 3) with
                    | ValueSome v -> vWin1 <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Win2")) then
                    match FsharpJsonHelpers.readMoney(&reader, 3) with
                    | ValueSome v -> vWin2 <- v
                    | ValueNone -> ()
                else reader.Skip()
            ValueSome {
                Value = vValue
                Win1 = vWin1
                Win2 = vWin2
            }
        else ValueNone
    static member MarketFromJson(reader: byref<Utf8JsonReader>): Test.Domain.Market voption =
        if FsharpJsonHelpers.moveToStartObject(&reader)then
            let mutable y = Test.Domain.Market.Unknown
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("ThreeWaysMarket")) then
                    let mutable _p1 = Test.Domain.ThreeWaysMarket.Default.Value
                    match ConvertTestDomain.ThreeWaysMarketFromJson(&reader) with
                    | ValueSome v -> _p1 <- v
                    | ValueNone -> ()
                    y <- _p1 |> Test.Domain.Market.ThreeWaysMarket
                else if (reader.ValueTextEquals("TwoWaysMarket")) then
                    let mutable _p1 = Test.Domain.TwoWaysMarket.Default.Value
                    match ConvertTestDomain.TwoWaysMarketFromJson(&reader) with
                    | ValueSome v -> _p1 <- v
                    | ValueNone -> ()
                    y <- _p1 |> Test.Domain.Market.TwoWaysMarket
                else if (reader.ValueTextEquals("HandicapMarket")) then
                    let mutable _p1 = Test.Domain.HandicapMarket.Default.Value
                    match ConvertTestDomain.HandicapMarketFromJson(&reader) with
                    | ValueSome v -> _p1 <- v
                    | ValueNone -> ()
                    y <- _p1 |> Test.Domain.Market.HandicapMarket
                else reader.Skip()
            ValueSome y
        else ValueNone
    static member MarketToJson (writer:inref<Utf8JsonWriter>, x: Test.Domain.Market) =
        writer.WriteStartObject()
        match x with
        | Test.Domain.Market.ThreeWaysMarket (p1) ->
            writer.WritePropertyName("ThreeWaysMarket")
            ConvertTestDomain.ThreeWaysMarketToJson(&writer, p1)
        | Test.Domain.Market.TwoWaysMarket (p1) ->
            writer.WritePropertyName("TwoWaysMarket")
            ConvertTestDomain.TwoWaysMarketToJson(&writer, p1)
        | Test.Domain.Market.HandicapMarket (p1) ->
            writer.WritePropertyName("HandicapMarket")
            ConvertTestDomain.HandicapMarketToJson(&writer, p1)
        | _ ->
            writer.WritePropertyName("Unknown")
            writer.WriteBooleanValue(true)
        writer.WriteEndObject()

    static member ScoreToJson (writer: inref<Utf8JsonWriter>, x: Test.Domain.Score) =
        writer.WriteStartObject()
        writer.WritePropertyName("S1")
        writer.WriteNumberValue(x.S1)
        writer.WritePropertyName("S2")
        writer.WriteNumberValue(x.S2)
        writer.WriteEndObject()

    static member ScoreFromJson(reader: byref<Utf8JsonReader>): Test.Domain.Score voption =
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
    static member GoalDetailsToJson (writer: inref<Utf8JsonWriter>, x: Test.Domain.GoalDetails) =
        writer.WriteStartObject()
        writer.WritePropertyName("Score")
        ConvertTestDomain.ScoreToJson(&writer, x.Score)
        writer.WritePropertyName("Comment")
        writer.WriteStringValue(x.Comment)
        writer.WriteEndObject()

    static member GoalDetailsFromJson(reader: byref<Utf8JsonReader>): Test.Domain.GoalDetails voption =
        let mutable vScore = Test.Domain.Score.Default.Value
        let mutable vComment = ""
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Score")) then
                    match ConvertTestDomain.ScoreFromJson(&reader) with
                    | ValueSome v -> vScore <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Comment")) then
                    match FsharpJsonHelpers.readString(&reader) with
                    | ValueSome v -> vComment <- v
                    | ValueNone -> ()
                else reader.Skip()
            ValueSome {
                Score = vScore
                Comment = vComment
            }
        else ValueNone
    static member MatchProtocolToJson (writer: inref<Utf8JsonWriter>, x: Test.Domain.MatchProtocol) =
        writer.WriteStartObject()
        writer.WritePropertyName("Details")
        writer.WriteStartArray()
        for v in x.Details do
            ConvertTestDomain.GoalDetailsToJson(&writer, v)
        writer.WriteEndArray()
        writer.WriteEndObject()

    static member MatchProtocolFromJson(reader: byref<Utf8JsonReader>): Test.Domain.MatchProtocol voption =
        let mutable vDetails = ResizeArray()
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Details")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.StartArray then
                        while reader.TokenType <> JsonTokenType.EndArray do
                            match ConvertTestDomain.GoalDetailsFromJson(&reader) with
                            | ValueSome v -> vDetails.Add(v)
                            | ValueNone -> ()
                    else reader.Skip()
                else reader.Skip()
            ValueSome {
                Details = vDetails |> List.ofSeq
            }
        else ValueNone
