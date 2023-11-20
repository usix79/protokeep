namespace Test.Converters

open MongoDB.Bson
open MongoDB.Bson.IO
open MongoDB.Bson.Serialization
open MongoDB.Bson.Serialization.Serializers
open Protokeep

type ConvertTestDomain() =
    static member ThreeWaysMarketToBson(writer: IBsonWriter, x: Test.Domain.ThreeWaysMarket) =
        writer.WriteStartDocument()
        writer.WriteName("Win1")
        writer.WriteInt32(FsharpMongoHelpers.toMoney (x.Win1, 3))
        writer.WriteName("Draw")
        writer.WriteInt32(FsharpMongoHelpers.toMoney (x.Draw, 3))
        writer.WriteName("Win2")
        writer.WriteInt32(FsharpMongoHelpers.toMoney (x.Win2, 3))
        writer.WriteEndDocument()

    static member ThreeWaysMarketFromBson(reader: IBsonReader): Test.Domain.ThreeWaysMarket =
        let mutable vWin1 = 0m
        let mutable vDraw = 0m
        let mutable vWin2 = 0m
        reader.ReadStartDocument()
        while reader.State <> BsonReaderState.EndOfDocument do
            match reader.State with
            | BsonReaderState.Type -> reader.ReadBsonType() |> ignore
            | BsonReaderState.Name ->
                match reader.ReadName() with
                | "Win1" ->
                    match FsharpMongoHelpers.readMoney(reader, 3) with
                    | ValueSome v -> vWin1 <- v
                    | ValueNone -> ()
                | "Draw" ->
                    match FsharpMongoHelpers.readMoney(reader, 3) with
                    | ValueSome v -> vDraw <- v
                    | ValueNone -> ()
                | "Win2" ->
                    match FsharpMongoHelpers.readMoney(reader, 3) with
                    | ValueSome v -> vWin2 <- v
                    | ValueNone -> ()
                | _ -> reader.SkipValue()
            | _ -> printfn "Unexpected state: %A" reader.State
        reader.ReadEndDocument()
        {
            Win1 = vWin1
            Draw = vDraw
            Win2 = vWin2
        }

    static member TwoWaysMarketToBson(writer: IBsonWriter, x: Test.Domain.TwoWaysMarket) =
        writer.WriteStartDocument()
        writer.WriteName("Win1")
        writer.WriteInt32(FsharpMongoHelpers.toMoney (x.Win1, 3))
        writer.WriteName("Win2")
        writer.WriteInt32(FsharpMongoHelpers.toMoney (x.Win2, 3))
        writer.WriteEndDocument()

    static member TwoWaysMarketFromBson(reader: IBsonReader): Test.Domain.TwoWaysMarket =
        let mutable vWin1 = 0m
        let mutable vWin2 = 0m
        reader.ReadStartDocument()
        while reader.State <> BsonReaderState.EndOfDocument do
            match reader.State with
            | BsonReaderState.Type -> reader.ReadBsonType() |> ignore
            | BsonReaderState.Name ->
                match reader.ReadName() with
                | "Win1" ->
                    match FsharpMongoHelpers.readMoney(reader, 3) with
                    | ValueSome v -> vWin1 <- v
                    | ValueNone -> ()
                | "Win2" ->
                    match FsharpMongoHelpers.readMoney(reader, 3) with
                    | ValueSome v -> vWin2 <- v
                    | ValueNone -> ()
                | _ -> reader.SkipValue()
            | _ -> printfn "Unexpected state: %A" reader.State
        reader.ReadEndDocument()
        {
            Win1 = vWin1
            Win2 = vWin2
        }

    static member HandicapMarketToBson(writer: IBsonWriter, x: Test.Domain.HandicapMarket) =
        writer.WriteStartDocument()
        writer.WriteName("Value")
        writer.WriteInt32(FsharpMongoHelpers.toMoney (x.Value, 2))
        writer.WriteName("Win1")
        writer.WriteInt32(FsharpMongoHelpers.toMoney (x.Win1, 3))
        writer.WriteName("Win2")
        writer.WriteInt32(FsharpMongoHelpers.toMoney (x.Win2, 3))
        writer.WriteEndDocument()

    static member HandicapMarketFromBson(reader: IBsonReader): Test.Domain.HandicapMarket =
        let mutable vValue = 0m
        let mutable vWin1 = 0m
        let mutable vWin2 = 0m
        reader.ReadStartDocument()
        while reader.State <> BsonReaderState.EndOfDocument do
            match reader.State with
            | BsonReaderState.Type -> reader.ReadBsonType() |> ignore
            | BsonReaderState.Name ->
                match reader.ReadName() with
                | "Value" ->
                    match FsharpMongoHelpers.readMoney(reader, 2) with
                    | ValueSome v -> vValue <- v
                    | ValueNone -> ()
                | "Win1" ->
                    match FsharpMongoHelpers.readMoney(reader, 3) with
                    | ValueSome v -> vWin1 <- v
                    | ValueNone -> ()
                | "Win2" ->
                    match FsharpMongoHelpers.readMoney(reader, 3) with
                    | ValueSome v -> vWin2 <- v
                    | ValueNone -> ()
                | _ -> reader.SkipValue()
            | _ -> printfn "Unexpected state: %A" reader.State
        reader.ReadEndDocument()
        {
            Value = vValue
            Win1 = vWin1
            Win2 = vWin2
        }

    static member MarketToBson(writer: IBsonWriter, x: Test.Domain.Market) =
        writer.WriteStartDocument()
        match x with
        | Test.Domain.Market.ThreeWaysMarket (p1) ->
            writer.WriteName("ThreeWaysMarket")
            ConvertTestDomain.ThreeWaysMarketToBson(writer, p1)
        | Test.Domain.Market.TwoWaysMarket (p1) ->
            writer.WriteName("TwoWaysMarket")
            ConvertTestDomain.TwoWaysMarketToBson(writer, p1)
        | Test.Domain.Market.HandicapMarket (p1) ->
            writer.WriteName("HandicapMarket")
            ConvertTestDomain.HandicapMarketToBson(writer, p1)
        | _ ->
            writer.WriteName("Unknown")
            writer.WriteBoolean(true)
        writer.WriteEndDocument()
    static member MarketFromBson (reader: IBsonReader): Test.Domain.Market =
        let mutable y = Test.Domain.Market.Unknown
        reader.ReadStartDocument()
        match reader.ReadName() with
                | "ThreeWaysMarket" ->
                    let mutable _p1 = Test.Domain.ThreeWaysMarket.Default.Value
                    match ConvertTestDomain.ThreeWaysMarketFromBson(reader) |> ValueSome with
                    | ValueSome v -> _p1 <- v
                    | ValueNone -> ()
                    y <- _p1 |> Test.Domain.Market.ThreeWaysMarket
                | "TwoWaysMarket" ->
                    let mutable _p1 = Test.Domain.TwoWaysMarket.Default.Value
                    match ConvertTestDomain.TwoWaysMarketFromBson(reader) |> ValueSome with
                    | ValueSome v -> _p1 <- v
                    | ValueNone -> ()
                    y <- _p1 |> Test.Domain.Market.TwoWaysMarket
                | "HandicapMarket" ->
                    let mutable _p1 = Test.Domain.HandicapMarket.Default.Value
                    match ConvertTestDomain.HandicapMarketFromBson(reader) |> ValueSome with
                    | ValueSome v -> _p1 <- v
                    | ValueNone -> ()
                    y <- _p1 |> Test.Domain.Market.HandicapMarket
                | _ -> reader.SkipValue()
        reader.ReadEndDocument()
        y
    static member ScoreToBson(writer: IBsonWriter, x: Test.Domain.Score, ?asEntity: bool) =
        writer.WriteStartDocument()
        if asEntity.IsSome then
            FsharpMongoHelpers.writeId (writer, x)
        writer.WriteName("S1")
        writer.WriteInt32(x.S1)
        writer.WriteName("S2")
        writer.WriteInt32(x.S2)
        writer.WriteEndDocument()

    static member ScoreFromBson(reader: IBsonReader): Test.Domain.Score =
        let mutable vS1 = 0
        let mutable vS2 = 0
        reader.ReadStartDocument()
        while reader.State <> BsonReaderState.EndOfDocument do
            match reader.State with
            | BsonReaderState.Type -> reader.ReadBsonType() |> ignore
            | BsonReaderState.Name ->
                match reader.ReadName() with
                | "S1" ->
                    match FsharpMongoHelpers.readInt32 reader with
                    | ValueSome v -> vS1 <- v
                    | ValueNone -> ()
                | "S2" ->
                    match FsharpMongoHelpers.readInt32 reader with
                    | ValueSome v -> vS2 <- v
                    | ValueNone -> ()
                | _ -> reader.SkipValue()
            | _ -> printfn "Unexpected state: %A" reader.State
        reader.ReadEndDocument()
        {
            S1 = vS1
            S2 = vS2
        }

    static member GoalDetailsToBson(writer: IBsonWriter, x: Test.Domain.GoalDetails) =
        writer.WriteStartDocument()
        writer.WriteName("Score")
        ConvertTestDomain.ScoreToBson(writer, x.Score)
        writer.WriteName("Comment")
        writer.WriteString(x.Comment)
        writer.WriteEndDocument()

    static member GoalDetailsFromBson(reader: IBsonReader): Test.Domain.GoalDetails =
        let mutable vScore = Test.Domain.Score.Default.Value
        let mutable vComment = ""
        reader.ReadStartDocument()
        while reader.State <> BsonReaderState.EndOfDocument do
            match reader.State with
            | BsonReaderState.Type -> reader.ReadBsonType() |> ignore
            | BsonReaderState.Name ->
                match reader.ReadName() with
                | "Score" ->
                    match ConvertTestDomain.ScoreFromBson(reader) |> ValueSome with
                    | ValueSome v -> vScore <- v
                    | ValueNone -> ()
                | "Comment" ->
                    match FsharpMongoHelpers.readString reader with
                    | ValueSome v -> vComment <- v
                    | ValueNone -> ()
                | _ -> reader.SkipValue()
            | _ -> printfn "Unexpected state: %A" reader.State
        reader.ReadEndDocument()
        {
            Score = vScore
            Comment = vComment
        }

    static member MatchProtocolToBson(writer: IBsonWriter, x: Test.Domain.MatchProtocol) =
        writer.WriteStartDocument()
        writer.WriteName("Details")
        writer.WriteStartArray()
        for v in x.Details do
            ConvertTestDomain.GoalDetailsToBson(writer, v)
        writer.WriteEndArray()
        writer.WriteEndDocument()

    static member MatchProtocolFromBson(reader: IBsonReader): Test.Domain.MatchProtocol =
        let mutable vDetails = ResizeArray()
        reader.ReadStartDocument()
        while reader.State <> BsonReaderState.EndOfDocument do
            match reader.State with
            | BsonReaderState.Type -> reader.ReadBsonType() |> ignore
            | BsonReaderState.Name ->
                match reader.ReadName() with
                | "Details" ->
                    reader.ReadStartArray()
                    while reader.ReadBsonType() <> BsonType.EndOfDocument do
                        match ConvertTestDomain.GoalDetailsFromBson(reader) |> ValueSome with
                        | ValueSome v -> vDetails.Add(v)
                        | ValueNone -> ()
                    reader.ReadEndArray()
                | _ -> reader.SkipValue()
            | _ -> printfn "Unexpected state: %A" reader.State
        reader.ReadEndDocument()
        {
            Details = vDetails |> List.ofSeq
        }


type ScoreSerializer() =
    inherit SerializerBase<Test.Domain.Score>()
    override x.Deserialize(ctx: BsonDeserializationContext, args: BsonDeserializationArgs) =
        ConvertTestDomain.ScoreFromBson(ctx.Reader)

    override x.Serialize(ctx: BsonSerializationContext, args: BsonSerializationArgs, value: Test.Domain.Score) =
        ConvertTestDomain.ScoreToBson(ctx.Writer, value, true)

type ConvertTestDomain with
    static member RegisterSerializers() =
        BsonDefaults.GuidRepresentationMode <- GuidRepresentationMode.V3
        BsonSerializer.RegisterSerializer(GuidSerializer(GuidRepresentation.Standard))
        BsonSerializer.RegisterSerializer(ScoreSerializer())
