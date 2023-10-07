namespace Protokeep.FsharpFable
open Fable.SimpleJson
open Protokeep

type ConvertBetting () =
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
        FsharpFableHelpers.getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Empty" -> pair.Value |> FsharpFableHelpers.ifBool (fun v -> y <- Betting.Outcome.Empty)
            | "Priced" -> pair.Value |> FsharpFableHelpers.ifNumber (fun v -> y <- v |> unbox |> Betting.Outcome.Priced)
            | "PricedWithProb" -> pair.Value |> (fun v -> y <- v |> ConvertBetting.OutcomeCasePricedWithProbFromJson)
            | "Resulted" -> pair.Value |> FsharpFableHelpers.ifString (fun v -> y <- v |> ConvertBetting.OutcomeResultFromString |> Betting.Outcome.Resulted)
            | _ -> () )
        y
    static member OutcomeToJson (x:Betting.Outcome) =
        match x with
        | Betting.Outcome.Empty -> "Empty", JBool (true)
        | Betting.Outcome.Priced (price) -> "Priced", JNumber (unbox price)
        | Betting.Outcome.PricedWithProb (price,prob) -> "PricedWithProb", ConvertBetting.OutcomeCasePricedWithProbToJson (price,prob)
        | Betting.Outcome.Resulted (result) -> "Resulted", JString (result |> ConvertBetting.OutcomeResultToString)
        | _ -> "Unknown", JBool (true)
        |> List.singleton |> Map.ofList |> JObject
    static member OutcomeCasePricedWithProbFromJson (json: Json) =
        let mutable price = 0m
        let mutable prob = 0.f
        FsharpFableHelpers.getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Price" -> pair.Value |> FsharpFableHelpers.ifNumber (fun v -> price <- v |> unbox)
            | "Prob" -> pair.Value |> FsharpFableHelpers.ifNumber (fun v -> prob <- v |> unbox)
            | _ -> () )
        Betting.Outcome.PricedWithProb (price,prob)
    static member OutcomeCasePricedWithProbToJson (price,prob) =
        [
           "Price", JNumber (unbox price)
           "Prob", JNumber (unbox prob)
        ] |> Map.ofList |> JObject
    static member Winner3WayFromJson (json: Json): Betting.Winner3Way =
        let mutable vWin1 = Betting.Outcome.Unknown
        let mutable vDraw = Betting.Outcome.Unknown
        let mutable vWin2 = Betting.Outcome.Unknown
        FsharpFableHelpers.getProps json
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
    static member HandicapFromJson (json: Json): Betting.Handicap =
        let mutable vValue = 0m
        let mutable vWin1 = Betting.Outcome.Unknown
        let mutable vWin2 = Betting.Outcome.Unknown
        FsharpFableHelpers.getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Value" -> pair.Value |> FsharpFableHelpers.ifNumber (fun v -> vValue <- v |> unbox)
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
           "Value", JNumber (unbox x.Value)
           "Win1", (x.Win1 |> ConvertBetting.OutcomeToJson)
           "Win2", (x.Win2 |> ConvertBetting.OutcomeToJson)
        ] |> Map.ofList |> JObject
    static member TotalFromJson (json: Json): Betting.Total =
        let mutable vValue = 0m
        let mutable vOver = Betting.Outcome.Unknown
        let mutable vUnder = Betting.Outcome.Unknown
        FsharpFableHelpers.getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Value" -> pair.Value |> FsharpFableHelpers.ifNumber (fun v -> vValue <- v |> unbox)
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
           "Value", JNumber (unbox x.Value)
           "Over", (x.Over |> ConvertBetting.OutcomeToJson)
           "Under", (x.Under |> ConvertBetting.OutcomeToJson)
        ] |> Map.ofList |> JObject
    static member ScoreFromJson (json: Json): Betting.Score =
        let mutable vS1 = 0
        let mutable vS2 = 0
        FsharpFableHelpers.getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "S1" -> pair.Value |> FsharpFableHelpers.ifNumber (fun v -> vS1 <- v |> unbox)
            | "S2" -> pair.Value |> FsharpFableHelpers.ifNumber (fun v -> vS2 <- v |> unbox)
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
    static member ScoreOutcomeFromJson (json: Json): Betting.ScoreOutcome =
        let mutable vScore = ConvertBetting.DefaultScore.Value
        let mutable vOutcome = Betting.Outcome.Unknown
        FsharpFableHelpers.getProps json
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
    static member CorrectScoreFromJson (json: Json): Betting.CorrectScore =
        let mutable vScores = ResizeArray()
        FsharpFableHelpers.getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Scores" -> pair.Value |> FsharpFableHelpers.ifArray (Seq.iter ((fun v -> v |> ConvertBetting.ScoreOutcomeFromJson |> vScores.Add)))
            | _ -> () )
        {
            Scores = vScores |> List.ofSeq
        }
    static member CorrectScoreToJson (x: Betting.CorrectScore) =
        [
           "Scores", JArray (x.Scores |> Seq.map (fun v -> (v |> ConvertBetting.ScoreOutcomeToJson)) |> List.ofSeq)
        ] |> Map.ofList |> JObject
