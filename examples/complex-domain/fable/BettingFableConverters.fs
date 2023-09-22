namespace Protokeep.FableConverters
open Fable.SimpleJson
open Protokeep.FableConverterHelpers
type ConvertBetting () =
    static member DefaultOutcomeResult =
        lazy Betting.OutcomeResult.Unknown
    static member OutcomeResultFromString = function
        | "OutcomeResultWin" -> Betting.OutcomeResult.Win
        | "OutcomeResultLose" -> Betting.OutcomeResult.Lose
        | "OutcomeResultVoid" -> Betting.OutcomeResult.Void
        | "OutcomeResultCanceled" -> Betting.OutcomeResult.Canceled
        | _ -> Betting.OutcomeResult.Unknown
    static member OutcomeResultToString = function
        | Betting.OutcomeResult.Win -> "OutcomeResultWin"
        | Betting.OutcomeResult.Lose -> "OutcomeResultLose"
        | Betting.OutcomeResult.Void -> "OutcomeResultVoid"
        | Betting.OutcomeResult.Canceled -> "OutcomeResultCanceled"
        | _ -> "Unknown"
    static member OutcomeFromJson (json: Json): Betting.Outcome =
        let mutable y = Betting.Outcome.Unknown
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Empty" -> pair.Value |> ifBool (fun v -> y <- Betting.Outcome.Empty)
            | "Priced" -> pair.Value |> ifNumber (fun v -> y <- v / 1000. |> unbox |> Betting.Outcome.Priced)
            | "PricedWithProb" -> pair.Value |> (fun v -> y <- v |> ConvertBetting.OutcomeCasePricedWithProbFromJson)
            | "Resulted" -> pair.Value |> ifString (fun v -> y <- v |> ConvertBetting.OutcomeResultFromString |> Betting.Outcome.Resulted)
            | _ -> () )
        y
    static member OutcomeToJson (x:Betting.Outcome) =
        match x with
        | Betting.Outcome.Empty -> "Empty", JBool (true)
        | Betting.Outcome.Priced (price) -> "Priced", JNumber (price * 1000m |> System.Decimal.Truncate |> unbox)
        | Betting.Outcome.PricedWithProb (price,prob) -> "PricedWithProb", ConvertBetting.OutcomeCasePricedWithProbToJson (price,prob)
        | Betting.Outcome.Resulted (result) -> "Resulted", JString (result |> ConvertBetting.OutcomeResultToString)
        | _ -> "Unknown", JBool (true)
        |> List.singleton |> Map.ofList |> JObject
    static member OutcomeCasePricedWithProbFromJson (json: Json) =
        let mutable price = 0m
        let mutable prob = 0.f
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Price" -> pair.Value |> ifNumber (fun v -> price <- v / 1000. |> unbox)
            | "Prob" -> pair.Value |> ifNumber (fun v -> prob <- v |> unbox)
            | _ -> () )
        Betting.Outcome.PricedWithProb (price,prob)
    static member OutcomeCasePricedWithProbToJson (price,prob) =
        [
           "Price", JNumber (price * 1000m |> System.Decimal.Truncate |> unbox)
           "Prob", JNumber (unbox prob)
        ] |> Map.ofList |> JObject
    static member DefaultWinner3Way: Lazy<Betting.Winner3Way> =
        lazy {
            Win1 = Betting.Outcome.Unknown
            Draw = Betting.Outcome.Unknown
            Win2 = Betting.Outcome.Unknown
        }
    static member Winner3WayFromJson (json: Json): Betting.Winner3Way =
        let mutable vWin1 = Betting.Outcome.Unknown
        let mutable vDraw = Betting.Outcome.Unknown
        let mutable vWin2 = Betting.Outcome.Unknown
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Win1" -> pair.Value |> (fun v -> vWin1 <- v |> ConvertBetting.OutcomeFromJson)
            | "Draw" -> pair.Value |> (fun v -> vDraw <- v |> ConvertBetting.OutcomeFromJson)
            | "Win2" -> pair.Value |> (fun v -> vWin2 <- v |> ConvertBetting.OutcomeFromJson)
            | _ -> () )
        {
            Win1 = vWin1
            Draw = vDraw
            Win2 = vWin2
        }
    static member Winner3WayToJson (x: Betting.Winner3Way) =
        [
           "Win1", (x.Win1 |> ConvertBetting.OutcomeToJson)
           "Draw", (x.Draw |> ConvertBetting.OutcomeToJson)
           "Win2", (x.Win2 |> ConvertBetting.OutcomeToJson)
        ] |> Map.ofList |> JObject
    static member DefaultHandicap: Lazy<Betting.Handicap> =
        lazy {
            Value = 0m
            Win1 = Betting.Outcome.Unknown
            Win2 = Betting.Outcome.Unknown
        }
    static member HandicapFromJson (json: Json): Betting.Handicap =
        let mutable vValue = 0m
        let mutable vWin1 = Betting.Outcome.Unknown
        let mutable vWin2 = Betting.Outcome.Unknown
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Value" -> pair.Value |> ifNumber (fun v -> vValue <- v / 100. |> unbox)
            | "Win1" -> pair.Value |> (fun v -> vWin1 <- v |> ConvertBetting.OutcomeFromJson)
            | "Win2" -> pair.Value |> (fun v -> vWin2 <- v |> ConvertBetting.OutcomeFromJson)
            | _ -> () )
        {
            Value = vValue
            Win1 = vWin1
            Win2 = vWin2
        }
    static member HandicapToJson (x: Betting.Handicap) =
        [
           "Value", JNumber (x.Value * 100m |> System.Decimal.Truncate |> unbox)
           "Win1", (x.Win1 |> ConvertBetting.OutcomeToJson)
           "Win2", (x.Win2 |> ConvertBetting.OutcomeToJson)
        ] |> Map.ofList |> JObject
    static member DefaultTotal: Lazy<Betting.Total> =
        lazy {
            Value = 0m
            Over = Betting.Outcome.Unknown
            Under = Betting.Outcome.Unknown
        }
    static member TotalFromJson (json: Json): Betting.Total =
        let mutable vValue = 0m
        let mutable vOver = Betting.Outcome.Unknown
        let mutable vUnder = Betting.Outcome.Unknown
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Value" -> pair.Value |> ifNumber (fun v -> vValue <- v / 100. |> unbox)
            | "Over" -> pair.Value |> (fun v -> vOver <- v |> ConvertBetting.OutcomeFromJson)
            | "Under" -> pair.Value |> (fun v -> vUnder <- v |> ConvertBetting.OutcomeFromJson)
            | _ -> () )
        {
            Value = vValue
            Over = vOver
            Under = vUnder
        }
    static member TotalToJson (x: Betting.Total) =
        [
           "Value", JNumber (x.Value * 100m |> System.Decimal.Truncate |> unbox)
           "Over", (x.Over |> ConvertBetting.OutcomeToJson)
           "Under", (x.Under |> ConvertBetting.OutcomeToJson)
        ] |> Map.ofList |> JObject
    static member DefaultScore: Lazy<Betting.Score> =
        lazy {
            S1 = 0
            S2 = 0
        }
    static member ScoreFromJson (json: Json): Betting.Score =
        let mutable vS1 = 0
        let mutable vS2 = 0
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "S1" -> pair.Value |> ifNumber (fun v -> vS1 <- v |> unbox)
            | "S2" -> pair.Value |> ifNumber (fun v -> vS2 <- v |> unbox)
            | _ -> () )
        {
            S1 = vS1
            S2 = vS2
        }
    static member ScoreToJson (x: Betting.Score) =
        [
           "S1", JNumber (unbox x.S1)
           "S2", JNumber (unbox x.S2)
        ] |> Map.ofList |> JObject
    static member DefaultScoreOutcome: Lazy<Betting.ScoreOutcome> =
        lazy {
            Score = ConvertBetting.DefaultScore.Value
            Outcome = Betting.Outcome.Unknown
        }
    static member ScoreOutcomeFromJson (json: Json): Betting.ScoreOutcome =
        let mutable vScore = ConvertBetting.DefaultScore.Value
        let mutable vOutcome = Betting.Outcome.Unknown
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Score" -> pair.Value |> (fun v -> vScore <- v |> ConvertBetting.ScoreFromJson)
            | "Outcome" -> pair.Value |> (fun v -> vOutcome <- v |> ConvertBetting.OutcomeFromJson)
            | _ -> () )
        {
            Score = vScore
            Outcome = vOutcome
        }
    static member ScoreOutcomeToJson (x: Betting.ScoreOutcome) =
        [
           "Score", (x.Score |> ConvertBetting.ScoreToJson)
           "Outcome", (x.Outcome |> ConvertBetting.OutcomeToJson)
        ] |> Map.ofList |> JObject
    static member DefaultCorrectScore: Lazy<Betting.CorrectScore> =
        lazy {
            Scores = List.empty
        }
    static member CorrectScoreFromJson (json: Json): Betting.CorrectScore =
        let mutable vScores = ResizeArray()
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Scores" -> pair.Value |> ifArray (Seq.iter ((fun v -> v |> ConvertBetting.ScoreOutcomeFromJson |> vScores.Add)))
            | _ -> () )
        {
            Scores = vScores |> List.ofSeq
        }
    static member CorrectScoreToJson (x: Betting.CorrectScore) =
        [
           "Scores", JArray (x.Scores |> Seq.map (fun v -> (v |> ConvertBetting.ScoreOutcomeToJson)) |> List.ofSeq)
        ] |> Map.ofList |> JObject
