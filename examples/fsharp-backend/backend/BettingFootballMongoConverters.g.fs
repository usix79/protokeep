namespace Protokeep.FsharpMongo

open MongoDB.Bson
open MongoDB.Bson.IO
open MongoDB.Bson.Serialization
open MongoDB.Bson.Serialization.Serializers
open Protokeep

type ConvertExampleBettingFootball() =
    static member MarketToBson(writer: IBsonWriter, x: Example.Betting.Football.Market) =
        writer.WriteStartDocument()
        match x with
        | Example.Betting.Football.Market.Winner3Way (p1) ->
            writer.WriteName("Winner3Way")
            ConvertExampleBetting.Winner3WayToBson(writer, p1)
        | Example.Betting.Football.Market.Handicap (p1) ->
            writer.WriteName("Handicap")
            ConvertExampleBetting.HandicapToBson(writer, p1)
        | Example.Betting.Football.Market.Total (p1) ->
            writer.WriteName("Total")
            ConvertExampleBetting.TotalToBson(writer, p1)
        | Example.Betting.Football.Market.CorrectScore (p1) ->
            writer.WriteName("CorrectScore")
            ConvertExampleBetting.CorrectScoreToBson(writer, p1)
        | _ ->
            writer.WriteName("Unknown")
            writer.WriteBoolean(true)
        writer.WriteEndDocument()
    static member MarketFromBson (reader: IBsonReader): Example.Betting.Football.Market =
        let mutable y = Example.Betting.Football.Market.Unknown
        reader.ReadStartDocument()
        match reader.ReadName() with
                | "Winner3Way" ->
                    let mutable _p1 = Example.Betting.Winner3Way.Default.Value
                    match ConvertExampleBetting.Winner3WayFromBson(reader) |> ValueSome with
                    | ValueSome v -> _p1 <- v
                    | ValueNone -> ()
                    y <- _p1 |> Example.Betting.Football.Market.Winner3Way
                | "Handicap" ->
                    let mutable _p1 = Example.Betting.Handicap.Default.Value
                    match ConvertExampleBetting.HandicapFromBson(reader) |> ValueSome with
                    | ValueSome v -> _p1 <- v
                    | ValueNone -> ()
                    y <- _p1 |> Example.Betting.Football.Market.Handicap
                | "Total" ->
                    let mutable _p1 = Example.Betting.Total.Default.Value
                    match ConvertExampleBetting.TotalFromBson(reader) |> ValueSome with
                    | ValueSome v -> _p1 <- v
                    | ValueNone -> ()
                    y <- _p1 |> Example.Betting.Football.Market.Total
                | "CorrectScore" ->
                    let mutable _p1 = Example.Betting.CorrectScore.Default.Value
                    match ConvertExampleBetting.CorrectScoreFromBson(reader) |> ValueSome with
                    | ValueSome v -> _p1 <- v
                    | ValueNone -> ()
                    y <- _p1 |> Example.Betting.Football.Market.CorrectScore
                | _ -> ()
        reader.ReadEndDocument()
        y
    static memberPeriodFromInt = function
        | Example.Betting.Football.Period.Half1 -> Example.Betting.Football.Period.Half1
        | Example.Betting.Football.Period.Half2 -> Example.Betting.Football.Period.Half2
        | Example.Betting.Football.Period.MainTime -> Example.Betting.Football.Period.MainTime
        | _ -> Example.Betting.Football.Period.Unknown

    static memberStatisticFromInt = function
        | Example.Betting.Football.Statistic.Goals -> Example.Betting.Football.Statistic.Goals
        | Example.Betting.Football.Statistic.YellowCards -> Example.Betting.Football.Statistic.YellowCards
        | Example.Betting.Football.Statistic.Corners -> Example.Betting.Football.Statistic.Corners
        | _ -> Example.Betting.Football.Statistic.Unknown

    static memberStatusFromInt = function
        | Example.Betting.Football.Status.Open -> Example.Betting.Football.Status.Open
        | Example.Betting.Football.Status.Closed -> Example.Betting.Football.Status.Closed
        | _ -> Example.Betting.Football.Status.Unknown

    static member MarketItemToBson(writer: IBsonWriter, x: Example.Betting.Football.MarketItem, ?asEntity: bool) =
        writer.WriteStartDocument()
        if asEntity.IsSome then
            FsharpMongoHelpers.writeId (writer, x)
        writer.WriteName("Statistic")
        writer.WriteInt32(x.Statistic |> int)
        writer.WriteName("Period")
        writer.WriteInt32(x.Period |> int)
        writer.WriteName("Market")
        ConvertExampleBettingFootball.MarketToBson(writer, x.Market)
        writer.WriteName("Status")
        writer.WriteInt32(x.Status |> int)
        writer.WriteName("Version")
        writer.WriteInt32(x.Version)
        writer.WriteEndDocument()

    static member MarketItemFromBson(reader: IBsonReader): Example.Betting.Football.MarketItem =
        let mutable vStatistic = Example.Betting.Football.Statistic.Unknown
        let mutable vPeriod = Example.Betting.Football.Period.Unknown
        let mutable vMarket = Example.Betting.Football.Market.Unknown
        let mutable vStatus = Example.Betting.Football.Status.Unknown
        let mutable vVersion = 0
        reader.ReadStartDocument()
        while reader.State <> BsonReaderState.EndOfDocument do
            match reader.State with
            | BsonReaderState.Type -> reader.ReadBsonType() |> ignore
            | BsonReaderState.Name ->
                match reader.ReadName() with
                | "Statistic" ->
                    match FsharpMongoHelpers.readInt reader |> ValueOption.map (fun v -> LanguagePrimitives.EnumOfValue (int v)) with
                    | ValueSome v -> vStatistic <- v
                    | ValueNone -> ()
                | "Period" ->
                    match FsharpMongoHelpers.readInt reader |> ValueOption.map (fun v -> LanguagePrimitives.EnumOfValue (int v)) with
                    | ValueSome v -> vPeriod <- v
                    | ValueNone -> ()
                | "Market" ->
                    match ConvertExampleBettingFootball.MarketFromBson(reader) |> ValueSome with
                    | ValueSome v -> vMarket <- v
                    | ValueNone -> ()
                | "Status" ->
                    match FsharpMongoHelpers.readInt reader |> ValueOption.map (fun v -> LanguagePrimitives.EnumOfValue (int v)) with
                    | ValueSome v -> vStatus <- v
                    | ValueNone -> ()
                | "Version" ->
                    match FsharpMongoHelpers.readInt32 reader with
                    | ValueSome v -> vVersion <- v
                    | ValueNone -> ()
                | _ -> reader.SkipValue()
            | _ -> printfn "Unexpected state: %A" reader.State
        reader.ReadEndDocument()
        {
            Statistic = vStatistic
            Period = vPeriod
            Market = vMarket
            Status = vStatus
            Version = vVersion
        }


type MarketItemSerializer() =
    inherit SerializerBase<Example.Betting.Football.MarketItem>()
    override x.Deserialize(ctx: BsonDeserializationContext, args: BsonDeserializationArgs) =
        ConvertExampleBettingFootball.MarketItemFromBson(ctx.Reader)

    override x.Serialize(ctx: BsonSerializationContext, args: BsonSerializationArgs, value: Example.Betting.Football.MarketItem) =
        ConvertExampleBettingFootball.MarketItemToBson(ctx.Writer, value, true)

type ConvertExampleBettingFootball with
    static member RegisterSerializers() =
        BsonDefaults.GuidRepresentationMode <- GuidRepresentationMode.V3
        BsonSerializer.RegisterSerializer(MarketItemSerializer())
