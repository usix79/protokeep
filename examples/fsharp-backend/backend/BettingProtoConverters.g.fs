namespace Protokeep.FsharpProto

type ConvertExampleBetting() =

    static member FromProtobuf(x: ProtoClasses.Example.Betting.OutcomeResult) : Example.Betting.OutcomeResult =
        LanguagePrimitives.EnumOfValue (sbyte x)

    static member ToProtobuf(x: Example.Betting.OutcomeResult) : ProtoClasses.Example.Betting.OutcomeResult =
        enum<ProtoClasses.Example.Betting.OutcomeResult> (int x)

    static member FromProtobuf(x: ProtoClasses.Example.Betting.Outcome) : Example.Betting.Outcome =
        match x.UnionCase with
        | ProtoClasses.Example.Betting.Outcome.UnionOneofCase.Empty -> Example.Betting.Outcome.Empty
        | ProtoClasses.Example.Betting.Outcome.UnionOneofCase.Priced -> Example.Betting.Outcome.Priced(x.Priced |> fun v -> (decimal v) / 1000m)
        | ProtoClasses.Example.Betting.Outcome.UnionOneofCase.PricedWithProb -> x.PricedWithProb |> ConvertExampleBetting.FromProtobuf
        | ProtoClasses.Example.Betting.Outcome.UnionOneofCase.Resulted -> Example.Betting.Outcome.Resulted(x.Resulted |> ConvertExampleBetting.FromProtobuf)
        | _ -> Example.Betting.Outcome.Unknown
    static member ToProtobuf(x: Example.Betting.Outcome) : ProtoClasses.Example.Betting.Outcome =
        let y = ProtoClasses.Example.Betting.Outcome()
        match x with
        | Example.Betting.Outcome.Empty -> y.Empty <- true
        | Example.Betting.Outcome.Priced (price) ->
            y.Priced <- price |> fun v -> int (v * 1000m)
        | Example.Betting.Outcome.PricedWithProb (price,prob) -> y.PricedWithProb <- ConvertExampleBetting.OutcomeCasePricedWithProbToProtobuf(price,prob)
        | Example.Betting.Outcome.Resulted (result) ->
            y.Resulted <- result |> ConvertExampleBetting.ToProtobuf
        | Example.Betting.Outcome.Unknown -> ()
        y
    static member FromProtobuf (x:ProtoClasses.Example.Betting.Outcome__PricedWithProb) =
        Example.Betting.Outcome.PricedWithProb
            ((x.Price |> fun v -> (decimal v) / 1000m),(x.Prob))

    static member OutcomeCasePricedWithProbToProtobuf (price,prob) : ProtoClasses.Example.Betting.Outcome__PricedWithProb =
        let y = ProtoClasses.Example.Betting.Outcome__PricedWithProb()
        y.Price <- price |> fun v -> int (v * 1000m)
        y.Prob <- prob
        y


    static member FromProtobuf(x: ProtoClasses.Example.Betting.Winner3Way) : Example.Betting.Winner3Way =
        {
            Win1 = x.Win1 |> ConvertExampleBetting.FromProtobuf
            Draw = x.Draw |> ConvertExampleBetting.FromProtobuf
            Win2 = x.Win2 |> ConvertExampleBetting.FromProtobuf
        }

    static member ToProtobuf(x: Example.Betting.Winner3Way) : ProtoClasses.Example.Betting.Winner3Way =
        let y = ProtoClasses.Example.Betting.Winner3Way()
        y.Win1 <- x.Win1 |> ConvertExampleBetting.ToProtobuf
        y.Draw <- x.Draw |> ConvertExampleBetting.ToProtobuf
        y.Win2 <- x.Win2 |> ConvertExampleBetting.ToProtobuf
        y

    static member FromProtobuf(x: ProtoClasses.Example.Betting.Handicap) : Example.Betting.Handicap =
        {
            Value = x.Value |> fun v -> (decimal v) / 100m
            Win1 = x.Win1 |> ConvertExampleBetting.FromProtobuf
            Win2 = x.Win2 |> ConvertExampleBetting.FromProtobuf
        }

    static member ToProtobuf(x: Example.Betting.Handicap) : ProtoClasses.Example.Betting.Handicap =
        let y = ProtoClasses.Example.Betting.Handicap()
        y.Value <- x.Value |> fun v -> int (v * 100m)
        y.Win1 <- x.Win1 |> ConvertExampleBetting.ToProtobuf
        y.Win2 <- x.Win2 |> ConvertExampleBetting.ToProtobuf
        y

    static member FromProtobuf(x: ProtoClasses.Example.Betting.Total) : Example.Betting.Total =
        {
            Value = x.Value |> fun v -> (decimal v) / 100m
            Over = x.Over |> ConvertExampleBetting.FromProtobuf
            Under = x.Under |> ConvertExampleBetting.FromProtobuf
        }

    static member ToProtobuf(x: Example.Betting.Total) : ProtoClasses.Example.Betting.Total =
        let y = ProtoClasses.Example.Betting.Total()
        y.Value <- x.Value |> fun v -> int (v * 100m)
        y.Over <- x.Over |> ConvertExampleBetting.ToProtobuf
        y.Under <- x.Under |> ConvertExampleBetting.ToProtobuf
        y

    static member FromProtobuf(x: ProtoClasses.Example.Betting.Score) : Example.Betting.Score =
        {
            S1 = x.S1 |> int16
            S2 = x.S2 |> int16
        }

    static member ToProtobuf(x: Example.Betting.Score) : ProtoClasses.Example.Betting.Score =
        let y = ProtoClasses.Example.Betting.Score()
        y.S1 <- x.S1 |> int
        y.S2 <- x.S2 |> int
        y

    static member FromProtobuf(x: ProtoClasses.Example.Betting.ScoreOutcome) : Example.Betting.ScoreOutcome =
        {
            Score = x.Score |> ConvertExampleBetting.FromProtobuf
            Outcome = x.Outcome |> ConvertExampleBetting.FromProtobuf
        }

    static member ToProtobuf(x: Example.Betting.ScoreOutcome) : ProtoClasses.Example.Betting.ScoreOutcome =
        let y = ProtoClasses.Example.Betting.ScoreOutcome()
        y.Score <- x.Score |> ConvertExampleBetting.ToProtobuf
        y.Outcome <- x.Outcome |> ConvertExampleBetting.ToProtobuf
        y

    static member FromProtobuf(x: ProtoClasses.Example.Betting.CorrectScore) : Example.Betting.CorrectScore =
        {
            Scores = x.Scores |> Seq.map(ConvertExampleBetting.FromProtobuf) |> List.ofSeq
        }

    static member ToProtobuf(x: Example.Betting.CorrectScore) : ProtoClasses.Example.Betting.CorrectScore =
        let y = ProtoClasses.Example.Betting.CorrectScore()
        y.Scores.AddRange(x.Scores |> Seq.map(ConvertExampleBetting.ToProtobuf))
        y

