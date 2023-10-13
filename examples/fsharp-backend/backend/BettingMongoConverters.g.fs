namespace Protokeep.FsharpMongo

open MongoDB.Bson
open MongoDB.Bson.IO
open MongoDB.Bson.Serialization
open MongoDB.Bson.Serialization.Serializers
open Protokeep

type ConvertExampleBetting() =
    static memberOutcomeResultFromInt = function
        | Example.Betting.OutcomeResult.Win -> Example.Betting.OutcomeResult.Win
        | Example.Betting.OutcomeResult.Lose -> Example.Betting.OutcomeResult.Lose
        | Example.Betting.OutcomeResult.Void -> Example.Betting.OutcomeResult.Void
        | Example.Betting.OutcomeResult.Canceled -> Example.Betting.OutcomeResult.Canceled
        | _ -> Example.Betting.OutcomeResult.Unknown

    static member OutcomeToBson(writer: IBsonWriter, x: Example.Betting.Outcome) =
        writer.WriteStartDocument()
        match x with
        | Example.Betting.Outcome.Empty ->
            writer.WriteName("Empty")
            writer.WriteBoolean(true)
        | Example.Betting.Outcome.Priced (price) ->
            writer.WriteName("Priced")
            writer.WriteInt32(FsharpMongoHelpers.toMoney (price, 3))
        | Example.Betting.Outcome.PricedWithProb (price,prob) ->
            writer.WriteName("PricedWithProb")
            ConvertExampleBetting.OutcomeCasePricedWithProbToBson(writer,price,prob)
        | Example.Betting.Outcome.Resulted (result) ->
            writer.WriteName("Resulted")
            writer.WriteInt32(result |> int)
        | _ ->
            writer.WriteName("Unknown")
            writer.WriteBoolean(true)
        writer.WriteEndDocument()
    static member OutcomeCasePricedWithProbToBson (writer: IBsonWriter,price,prob) =
        writer.WriteStartDocument()
        writer.WriteName("Price")
        writer.WriteInt32(FsharpMongoHelpers.toMoney (price, 3))
        writer.WriteName("Prob")
        writer.WriteDouble(double prob)
        writer.WriteEndDocument()
    static member OutcomeFromBson (reader: IBsonReader): Example.Betting.Outcome =
        let mutable y = Example.Betting.Outcome.Unknown
        reader.ReadStartDocument()
        match reader.ReadName() with
                | "Empty" ->
                    match FsharpMongoHelpers.readBoolean reader with
                    | ValueSome true -> y <- Example.Betting.Outcome.Empty
                    | _ -> ()
                | "Priced" ->
                    let mutable _price = 0m
                    match FsharpMongoHelpers.readMoney(reader, 3) with
                    | ValueSome v -> _price <- v
                    | ValueNone -> ()
                    y <- _price |> Example.Betting.Outcome.Priced
                | "PricedWithProb" ->
                    y <- ConvertExampleBetting.OutcomeCasePricedWithProbFromBson(reader)
                | "Resulted" ->
                    let mutable _result = Example.Betting.OutcomeResult.Unknown
                    match FsharpMongoHelpers.readInt reader |> ValueOption.map (fun v -> LanguagePrimitives.EnumOfValue (sbyte v)) with
                    | ValueSome v -> _result <- v
                    | ValueNone -> ()
                    y <- _result |> Example.Betting.Outcome.Resulted
                | _ -> ()
        reader.ReadEndDocument()
        y
    static member OutcomeCasePricedWithProbFromBson (reader: IBsonReader) =
        let mutable price = 0m
        let mutable prob = 0.f
        reader.ReadStartDocument()
        while reader.State <> BsonReaderState.EndOfDocument do
            match reader.State with
            | BsonReaderState.Type -> reader.ReadBsonType() |> ignore
            | BsonReaderState.Name ->
                match reader.ReadName() with
                | "Price" ->
                    match FsharpMongoHelpers.readMoney(reader, 3) with
                    | ValueSome v -> price <- v
                    | ValueNone -> ()
                | "Prob" ->
                    match FsharpMongoHelpers.readFloat32 reader with
                    | ValueSome v -> prob <- v
                    | ValueNone -> ()
                | _ -> reader.SkipValue()
            | _ -> printfn "Unexpected state: %A" reader.State
        reader.ReadEndDocument()
        Example.Betting.Outcome.PricedWithProb (price,prob)
    static member Winner3WayToBson(writer: IBsonWriter, x: Example.Betting.Winner3Way) =
        writer.WriteStartDocument()
        writer.WriteName("Win1")
        ConvertExampleBetting.OutcomeToBson(writer, x.Win1)
        writer.WriteName("Draw")
        ConvertExampleBetting.OutcomeToBson(writer, x.Draw)
        writer.WriteName("Win2")
        ConvertExampleBetting.OutcomeToBson(writer, x.Win2)
        writer.WriteEndDocument()

    static member Winner3WayFromBson(reader: IBsonReader): Example.Betting.Winner3Way =
        let mutable vWin1 = Example.Betting.Outcome.Unknown
        let mutable vDraw = Example.Betting.Outcome.Unknown
        let mutable vWin2 = Example.Betting.Outcome.Unknown
        reader.ReadStartDocument()
        while reader.State <> BsonReaderState.EndOfDocument do
            match reader.State with
            | BsonReaderState.Type -> reader.ReadBsonType() |> ignore
            | BsonReaderState.Name ->
                match reader.ReadName() with
                | "Win1" ->
                    match ConvertExampleBetting.OutcomeFromBson(reader) |> ValueSome with
                    | ValueSome v -> vWin1 <- v
                    | ValueNone -> ()
                | "Draw" ->
                    match ConvertExampleBetting.OutcomeFromBson(reader) |> ValueSome with
                    | ValueSome v -> vDraw <- v
                    | ValueNone -> ()
                | "Win2" ->
                    match ConvertExampleBetting.OutcomeFromBson(reader) |> ValueSome with
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

    static member HandicapToBson(writer: IBsonWriter, x: Example.Betting.Handicap, ?asEntity: bool) =
        writer.WriteStartDocument()
        if asEntity.IsSome then
            FsharpMongoHelpers.writeId (writer, x)
        writer.WriteName("Value")
        writer.WriteInt32(FsharpMongoHelpers.toMoney (x.Value, 2))
        writer.WriteName("Win1")
        ConvertExampleBetting.OutcomeToBson(writer, x.Win1)
        writer.WriteName("Win2")
        ConvertExampleBetting.OutcomeToBson(writer, x.Win2)
        writer.WriteEndDocument()

    static member HandicapFromBson(reader: IBsonReader): Example.Betting.Handicap =
        let mutable vValue = 0m
        let mutable vWin1 = Example.Betting.Outcome.Unknown
        let mutable vWin2 = Example.Betting.Outcome.Unknown
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
                    match ConvertExampleBetting.OutcomeFromBson(reader) |> ValueSome with
                    | ValueSome v -> vWin1 <- v
                    | ValueNone -> ()
                | "Win2" ->
                    match ConvertExampleBetting.OutcomeFromBson(reader) |> ValueSome with
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

    static member TotalToBson(writer: IBsonWriter, x: Example.Betting.Total, ?asEntity: bool) =
        writer.WriteStartDocument()
        if asEntity.IsSome then
            FsharpMongoHelpers.writeId (writer, x)
        writer.WriteName("Value")
        writer.WriteInt32(FsharpMongoHelpers.toMoney (x.Value, 2))
        writer.WriteName("Over")
        ConvertExampleBetting.OutcomeToBson(writer, x.Over)
        writer.WriteName("Under")
        ConvertExampleBetting.OutcomeToBson(writer, x.Under)
        writer.WriteEndDocument()

    static member TotalFromBson(reader: IBsonReader): Example.Betting.Total =
        let mutable vValue = 0m
        let mutable vOver = Example.Betting.Outcome.Unknown
        let mutable vUnder = Example.Betting.Outcome.Unknown
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
                | "Over" ->
                    match ConvertExampleBetting.OutcomeFromBson(reader) |> ValueSome with
                    | ValueSome v -> vOver <- v
                    | ValueNone -> ()
                | "Under" ->
                    match ConvertExampleBetting.OutcomeFromBson(reader) |> ValueSome with
                    | ValueSome v -> vUnder <- v
                    | ValueNone -> ()
                | _ -> reader.SkipValue()
            | _ -> printfn "Unexpected state: %A" reader.State
        reader.ReadEndDocument()
        {
            Value = vValue
            Over = vOver
            Under = vUnder
        }

    static member ScoreToBson(writer: IBsonWriter, x: Example.Betting.Score, ?asEntity: bool) =
        writer.WriteStartDocument()
        if asEntity.IsSome then
            FsharpMongoHelpers.writeId (writer, x)
        writer.WriteName("S1")
        writer.WriteInt32(int x.S1)
        writer.WriteName("S2")
        writer.WriteInt32(int x.S2)
        writer.WriteEndDocument()

    static member ScoreFromBson(reader: IBsonReader): Example.Betting.Score =
        let mutable vS1 = 0s
        let mutable vS2 = 0s
        reader.ReadStartDocument()
        while reader.State <> BsonReaderState.EndOfDocument do
            match reader.State with
            | BsonReaderState.Type -> reader.ReadBsonType() |> ignore
            | BsonReaderState.Name ->
                match reader.ReadName() with
                | "S1" ->
                    match FsharpMongoHelpers.readInt16 reader with
                    | ValueSome v -> vS1 <- v
                    | ValueNone -> ()
                | "S2" ->
                    match FsharpMongoHelpers.readInt16 reader with
                    | ValueSome v -> vS2 <- v
                    | ValueNone -> ()
                | _ -> reader.SkipValue()
            | _ -> printfn "Unexpected state: %A" reader.State
        reader.ReadEndDocument()
        {
            S1 = vS1
            S2 = vS2
        }

    static member ScoreOutcomeToBson(writer: IBsonWriter, x: Example.Betting.ScoreOutcome) =
        writer.WriteStartDocument()
        writer.WriteName("Score")
        ConvertExampleBetting.ScoreToBson(writer, x.Score)
        writer.WriteName("Outcome")
        ConvertExampleBetting.OutcomeToBson(writer, x.Outcome)
        writer.WriteEndDocument()

    static member ScoreOutcomeFromBson(reader: IBsonReader): Example.Betting.ScoreOutcome =
        let mutable vScore = Example.Betting.Score.Default.Value
        let mutable vOutcome = Example.Betting.Outcome.Unknown
        reader.ReadStartDocument()
        while reader.State <> BsonReaderState.EndOfDocument do
            match reader.State with
            | BsonReaderState.Type -> reader.ReadBsonType() |> ignore
            | BsonReaderState.Name ->
                match reader.ReadName() with
                | "Score" ->
                    match ConvertExampleBetting.ScoreFromBson(reader) |> ValueSome with
                    | ValueSome v -> vScore <- v
                    | ValueNone -> ()
                | "Outcome" ->
                    match ConvertExampleBetting.OutcomeFromBson(reader) |> ValueSome with
                    | ValueSome v -> vOutcome <- v
                    | ValueNone -> ()
                | _ -> reader.SkipValue()
            | _ -> printfn "Unexpected state: %A" reader.State
        reader.ReadEndDocument()
        {
            Score = vScore
            Outcome = vOutcome
        }

    static member CorrectScoreToBson(writer: IBsonWriter, x: Example.Betting.CorrectScore) =
        writer.WriteStartDocument()
        writer.WriteName("Scores")
        writer.WriteStartArray()
        for v in x.Scores do
            ConvertExampleBetting.ScoreOutcomeToBson(writer, v)
        writer.WriteEndArray()
        writer.WriteEndDocument()

    static member CorrectScoreFromBson(reader: IBsonReader): Example.Betting.CorrectScore =
        let mutable vScores = ResizeArray()
        reader.ReadStartDocument()
        while reader.State <> BsonReaderState.EndOfDocument do
            match reader.State with
            | BsonReaderState.Type -> reader.ReadBsonType() |> ignore
            | BsonReaderState.Name ->
                match reader.ReadName() with
                | "Scores" ->
                    reader.ReadStartArray()
                    while reader.ReadBsonType() <> BsonType.EndOfDocument do
                        match ConvertExampleBetting.ScoreOutcomeFromBson(reader) |> ValueSome with
                        | ValueSome v -> vScores.Add(v)
                        | ValueNone -> ()
                    reader.ReadEndArray()
                | _ -> reader.SkipValue()
            | _ -> printfn "Unexpected state: %A" reader.State
        reader.ReadEndDocument()
        {
            Scores = vScores |> List.ofSeq
        }


type HandicapSerializer() =
    inherit SerializerBase<Example.Betting.Handicap>()
    override x.Deserialize(ctx: BsonDeserializationContext, args: BsonDeserializationArgs) =
        ConvertExampleBetting.HandicapFromBson(ctx.Reader)

    override x.Serialize(ctx: BsonSerializationContext, args: BsonSerializationArgs, value: Example.Betting.Handicap) =
        ConvertExampleBetting.HandicapToBson(ctx.Writer, value, true)

type TotalSerializer() =
    inherit SerializerBase<Example.Betting.Total>()
    override x.Deserialize(ctx: BsonDeserializationContext, args: BsonDeserializationArgs) =
        ConvertExampleBetting.TotalFromBson(ctx.Reader)

    override x.Serialize(ctx: BsonSerializationContext, args: BsonSerializationArgs, value: Example.Betting.Total) =
        ConvertExampleBetting.TotalToBson(ctx.Writer, value, true)

type ScoreSerializer() =
    inherit SerializerBase<Example.Betting.Score>()
    override x.Deserialize(ctx: BsonDeserializationContext, args: BsonDeserializationArgs) =
        ConvertExampleBetting.ScoreFromBson(ctx.Reader)

    override x.Serialize(ctx: BsonSerializationContext, args: BsonSerializationArgs, value: Example.Betting.Score) =
        ConvertExampleBetting.ScoreToBson(ctx.Writer, value, true)

type ConvertExampleBetting with
    static member RegisterSerializers() =
        BsonDefaults.GuidRepresentationMode <- GuidRepresentationMode.V3
        BsonSerializer.RegisterSerializer(HandicapSerializer())
        BsonSerializer.RegisterSerializer(TotalSerializer())
        BsonSerializer.RegisterSerializer(ScoreSerializer())
