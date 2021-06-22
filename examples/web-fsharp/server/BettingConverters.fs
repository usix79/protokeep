namespace Protogen.FsharpConverters
type ConvertBetting () =
    static member FromProtobuf (x:ProtoClasses.Betting.OutcomeResult) : Betting.OutcomeResult =
        enum<Betting.OutcomeResult>(int x)
    static member ToProtobuf (x:Betting.OutcomeResult) : ProtoClasses.Betting.OutcomeResult =
        enum<ProtoClasses.Betting.OutcomeResult>(int x)
    static member FromProtobuf (x:ProtoClasses.Betting.Outcome) : Betting.Outcome =
        match x.UnionCase with
        | ProtoClasses.Betting.Outcome.UnionOneofCase.Empty -> Betting.Outcome.Empty
        | ProtoClasses.Betting.Outcome.UnionOneofCase.Priced -> Betting.Outcome.Priced(x.Priced |> fun v -> (decimal v) / 1000m)
        | ProtoClasses.Betting.Outcome.UnionOneofCase.PricedWithProb -> x.PricedWithProb |> ConvertBetting.FromProtobuf
        | ProtoClasses.Betting.Outcome.UnionOneofCase.Resulted -> Betting.Outcome.Resulted(x.Resulted |> ConvertBetting.FromProtobuf)
        | _ -> Betting.Outcome.Unknown
    static member ToProtobuf (x:Betting.Outcome) : ProtoClasses.Betting.Outcome =
        let y = ProtoClasses.Betting.Outcome()
        match x with
        | Betting.Outcome.Empty -> y.Empty <- true
        | Betting.Outcome.Priced (price) ->
            y.Priced <- price |> fun v -> int64(v * 1000m)
        | Betting.Outcome.PricedWithProb (price,prob) -> y.PricedWithProb <- ConvertBetting.OutcomeCasePricedWithProbToProtobuf(price,prob)
        | Betting.Outcome.Resulted (result) ->
            y.Resulted <- result |> ConvertBetting.ToProtobuf
        | Betting.Outcome.Unknown -> ()
        y
    static member FromProtobuf (x:ProtoClasses.Betting.Outcome__PricedWithProb)  =
        Betting.Outcome.PricedWithProb
            ((x.Price |> fun v -> (decimal v) / 1000m),(x.Prob))
    static member OutcomeCasePricedWithProbToProtobuf (price,prob) : ProtoClasses.Betting.Outcome__PricedWithProb =
        let y = ProtoClasses.Betting.Outcome__PricedWithProb()
        y.Price <- price |> fun v -> int64(v * 1000m)
        y.Prob <- prob
        y
    static member FromProtobuf (x:ProtoClasses.Betting.Winner3Way) : Betting.Winner3Way =
        {
            Win1 = x.Win1 |> ConvertBetting.FromProtobuf
            Draw = x.Draw |> ConvertBetting.FromProtobuf
            Win2 = x.Win2 |> ConvertBetting.FromProtobuf
        }
    static member ToProtobuf (x:Betting.Winner3Way) : ProtoClasses.Betting.Winner3Way =
        let y = ProtoClasses.Betting.Winner3Way()
        y.Win1 <- x.Win1 |> ConvertBetting.ToProtobuf
        y.Draw <- x.Draw |> ConvertBetting.ToProtobuf
        y.Win2 <- x.Win2 |> ConvertBetting.ToProtobuf
        y
    static member FromProtobuf (x:ProtoClasses.Betting.Handicap) : Betting.Handicap =
        {
            Value = x.Value |> fun v -> (decimal v) / 100m
            Win1 = x.Win1 |> ConvertBetting.FromProtobuf
            Win2 = x.Win2 |> ConvertBetting.FromProtobuf
        }
    static member ToProtobuf (x:Betting.Handicap) : ProtoClasses.Betting.Handicap =
        let y = ProtoClasses.Betting.Handicap()
        y.Value <- x.Value |> fun v -> int64(v * 100m)
        y.Win1 <- x.Win1 |> ConvertBetting.ToProtobuf
        y.Win2 <- x.Win2 |> ConvertBetting.ToProtobuf
        y
    static member FromProtobuf (x:ProtoClasses.Betting.Total) : Betting.Total =
        {
            Value = x.Value |> fun v -> (decimal v) / 100m
            Over = x.Over |> ConvertBetting.FromProtobuf
            Under = x.Under |> ConvertBetting.FromProtobuf
        }
    static member ToProtobuf (x:Betting.Total) : ProtoClasses.Betting.Total =
        let y = ProtoClasses.Betting.Total()
        y.Value <- x.Value |> fun v -> int64(v * 100m)
        y.Over <- x.Over |> ConvertBetting.ToProtobuf
        y.Under <- x.Under |> ConvertBetting.ToProtobuf
        y
    static member FromProtobuf (x:ProtoClasses.Betting.Score) : Betting.Score =
        {
            S1 = x.S1
            S2 = x.S2
        }
    static member ToProtobuf (x:Betting.Score) : ProtoClasses.Betting.Score =
        let y = ProtoClasses.Betting.Score()
        y.S1 <- x.S1
        y.S2 <- x.S2
        y
    static member FromProtobuf (x:ProtoClasses.Betting.ScoreOutcome) : Betting.ScoreOutcome =
        {
            Score = x.Score |> ConvertBetting.FromProtobuf
            Outcome = x.Outcome |> ConvertBetting.FromProtobuf
        }
    static member ToProtobuf (x:Betting.ScoreOutcome) : ProtoClasses.Betting.ScoreOutcome =
        let y = ProtoClasses.Betting.ScoreOutcome()
        y.Score <- x.Score |> ConvertBetting.ToProtobuf
        y.Outcome <- x.Outcome |> ConvertBetting.ToProtobuf
        y
    static member FromProtobuf (x:ProtoClasses.Betting.CorrectScore) : Betting.CorrectScore =
        {
            Scores = x.Scores |> Seq.map(ConvertBetting.FromProtobuf) |> List.ofSeq
        }
    static member ToProtobuf (x:Betting.CorrectScore) : ProtoClasses.Betting.CorrectScore =
        let y = ProtoClasses.Betting.CorrectScore()
        y.Scores.AddRange(x.Scores |> Seq.map(ConvertBetting.ToProtobuf))
        y
