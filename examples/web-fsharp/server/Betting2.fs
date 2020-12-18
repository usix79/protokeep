module rec Betting2

open Protogen.FsharpTypes

type OutcomeResult =
    | Unknown = 0
    | Win = 1
    | Lose = 2
    | Void = 3
    | Canceled = 4
type Outcome =
    | Unknown
    | Empty
    | Priced of price:decimal
    | PricedWithProb of price:decimal*prob:float32
    | Resulted of result:Betting2.OutcomeResult
type Winner3Way = {
    Win1 : Betting2.Outcome
    Draw : Betting2.Outcome
    Win2 : Betting2.Outcome
}
with
    member x.OutcomeValues () =
        [| x.Win1; x.Draw; x.Win2 |]
    member x.OutcomeIndexes () =
        [| Key.Value "1"; Key.Value "2"; Key.Value "3" |]
    member x.Outcome = function
        | Key.Value "1" -> Some x.Win1
        | Key.Value "2" -> Some x.Draw
        | Key.Value "3" -> Some x.Win2
        | _ -> None
    member x.WithOutcomes (items:Map<Key,Betting2.Outcome>) =
        {x with
            Win1 = items.TryFind(Key.Value "1") |> Option.defaultValue x.Win1
            Draw = items.TryFind(Key.Value "2") |> Option.defaultValue x.Draw
            Win2 = items.TryFind(Key.Value "3") |> Option.defaultValue x.Win2
        }

type Handicap = {
    Value : decimal
    Win1 : Betting2.Outcome
    Win2 : Betting2.Outcome
}
with
    static member MakeKey (value': decimal) =
        Key.Value (value'.ToString())
    member x.Key = Handicap.MakeKey x.Value
    member x.OutcomeValues () =
        [| x.Win1; x.Win2 |]
    member x.OutcomeIndexes () =
        [| Key.Value "1"; Key.Value "2" |]
    member x.Outcome = function
        | Key.Value "1" -> Some x.Win1
        | Key.Value "2" -> Some x.Win2
        | _ -> None
    member x.WithOutcomes (items:Map<Key,Betting2.Outcome>) =
        {x with
            Win1 = items.TryFind(Key.Value "1") |> Option.defaultValue x.Win1
            Win2 = items.TryFind(Key.Value "2") |> Option.defaultValue x.Win2
        }

type Total = {
    Value : decimal
    Over : Betting2.Outcome
    Under : Betting2.Outcome
}
with
    static member MakeKey (value': decimal) =
        Key.Value (value'.ToString())
    member x.Key = Total.MakeKey x.Value
    member x.OutcomeValues () =
        [| x.Over; x.Under |]
    member x.OutcomeIndexes () =
        [| Key.Value "1"; Key.Value "2" |]
    member x.Outcome = function
        | Key.Value "1" -> Some x.Over
        | Key.Value "2" -> Some x.Under
        | _ -> None
    member x.WithOutcomes (items:Map<Key,Betting2.Outcome>) =
        {x with
            Over = items.TryFind(Key.Value "1") |> Option.defaultValue x.Over
            Under = items.TryFind(Key.Value "2") |> Option.defaultValue x.Under
        }

type Score = {
    S1 : int
    S2 : int
}
with
    static member MakeKey (s1': int, s2': int) =
        Key.Items [Key.Value (s1'.ToString()); Key.Value (s2'.ToString())]
    member x.Key = Score.MakeKey (x.S1,x.S2)

type ScoreOutcome = {
    Score : Betting2.Score
    Outcome : Betting2.Outcome
}
with
    static member MakeKey (scoreKey:Key) =
        scoreKey
    member x.Key = ScoreOutcome.MakeKey (x.Score.Key)

type CorrectScore = {
    Scores : Betting2.ScoreOutcome array
}
with
    member x.OutcomeValues() =
        x.Scores |> Array.map (fun v -> v.Outcome)
    member x.OutcomeIndexes() =
        x.Scores |> Array.map (fun v -> v.Score.Key)
    member x.Outcome (key:Key) =
        x.Scores |> Array.tryFind (fun v -> v.Score.Key = key) |> Option.map (fun v -> v.Outcome)
    member x.WithOutcomes (items:Map<Key,Betting2.Outcome>) =
        {x with
            Scores = x.Scores |> Array.map (fun v -> items.TryFind v.Score.Key |> Option.map (fun i -> {v with Outcome = i}) |> Option.defaultValue v)
        }

type Market =
    | Unknown
    | Winner3Way of p1:Betting2.Winner3Way
    | Handicap of p1:Betting2.Handicap
    | Total of p1:Betting2.Total
    | CorrectScore of p1:Betting2.CorrectScore
with
    static member MakeUnknownKey () =  Key.Value "1"
    static member MakeWinner3WayKey () =  Key.Value "2"
    static member MakeHandicapKey (p1Key:Key) =  Key.Items [Key.Value "3";  Key.Inner p1Key]
    static member MakeTotalKey (p1Key:Key) =  Key.Items [Key.Value "4";  Key.Inner p1Key]
    static member MakeCorrectScoreKey () =  Key.Value "5"
    member x.Key =
        match x with
        | Betting2.Market.Unknown -> Market.MakeUnknownKey()
        | Betting2.Market.Winner3Way p1 -> Market.MakeWinner3WayKey ()
        | Betting2.Market.Handicap p1 -> Market.MakeHandicapKey (p1.Key)
        | Betting2.Market.Total p1 -> Market.MakeTotalKey (p1.Key)
        | Betting2.Market.CorrectScore p1 -> Market.MakeCorrectScoreKey ()
    member x.OutcomeValues() =
        match x with
        | Betting2.Market.Unknown -> Array.empty
        | Betting2.Market.Winner3Way p1 -> p1.OutcomeValues()
        | Betting2.Market.Handicap p1 -> p1.OutcomeValues()
        | Betting2.Market.Total p1 -> p1.OutcomeValues()
        | Betting2.Market.CorrectScore p1 -> p1.OutcomeValues()
    member x.OutcomeIndexes() =
        match x with
        | Betting2.Market.Unknown -> Array.empty
        | Betting2.Market.Winner3Way p1 -> p1.OutcomeIndexes()
        | Betting2.Market.Handicap p1 -> p1.OutcomeIndexes()
        | Betting2.Market.Total p1 -> p1.OutcomeIndexes()
        | Betting2.Market.CorrectScore p1 -> p1.OutcomeIndexes()
    member x.Outcome (key:Key) =
        match x with
        | Betting2.Market.Unknown -> None
        | Betting2.Market.Winner3Way p1 -> p1.Outcome key
        | Betting2.Market.Handicap p1 -> p1.Outcome key
        | Betting2.Market.Total p1 -> p1.Outcome key
        | Betting2.Market.CorrectScore p1 -> p1.Outcome key
    member x.WithOutcomes (items:Map<Key,Betting2.Outcome>) =
        match x with
        | Betting2.Market.Unknown -> x
        | Betting2.Market.Winner3Way p1 -> p1.WithOutcomes items |> Betting2.Market.Winner3Way
        | Betting2.Market.Handicap p1 -> p1.WithOutcomes items |> Betting2.Market.Handicap
        | Betting2.Market.Total p1 -> p1.WithOutcomes items |> Betting2.Market.Total
        | Betting2.Market.CorrectScore p1 -> p1.WithOutcomes items |> Betting2.Market.CorrectScore

type Period =
    | Unknown = 0
    | Half1 = 1
    | Half2 = 2
    | MainTime = 3
type Statistic =
    | Unknown = 0
    | Goals = 1
    | YellowCards = 2
    | Corners = 3
type Status =
    | Unknown = 0
    | Open = 1
    | Closed = 2
type MarketItem = {
    Statistic : Betting2.Statistic
    Period : Betting2.Period
    Market : Betting2.Market
    Status : Betting2.Status
}
with
    static member MakeKey (statistic':Betting2.Statistic, period':Betting2.Period, marketKey':Key) =
        Key.Items [
            Key.Value ((int statistic').ToString())
            Key.Value ((int period').ToString())
            Key.Inner marketKey'
        ]
    member x.Key = MarketItem.MakeKey(x.Statistic, x.Period, x.Market.Key)