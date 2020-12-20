module rec Betting
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
    | Resulted of result:Betting.OutcomeResult
with
    static member MakeUnknownKey () = Key.Value "0"
    static member MakeEmptyKey () = Key.Value "1"
    static member MakePricedKey () = Key.Value "2"
    static member MakePricedWithProbKey () = Key.Value "3"
    static member MakeResultedKey () = Key.Value "4"
    member x.Key =
        match x with
        | Unknown -> Outcome.MakeUnknownKey ()
        | Empty -> Outcome.MakeEmptyKey ()
        | Priced (price') -> Outcome.MakePricedKey ()
        | PricedWithProb (price', prob') -> Outcome.MakePricedWithProbKey ()
        | Resulted (result') -> Outcome.MakeResultedKey ()
type Winner3Way = {
    Win1 : Betting.Outcome
    Draw : Betting.Outcome
    Win2 : Betting.Outcome
}
with
    member x.ItemValues () =
        [| x.Win1; x.Draw; x.Win2 |]
    member x.ItemIndexes () =
        [| Key.Value "1"; Key.Value "2"; Key.Value "3" |]
    member x.Item = function
        | Key.Value "1" -> Some x.Win1
        | Key.Value "2" -> Some x.Draw
        | Key.Value "3" -> Some x.Win2
        | _ -> None
    member x.WithItems (items:Map<Key,_>) =
        {x with
            Win1 = items.TryFind(Key.Value "1") |> Option.defaultValue x.Win1
            Draw = items.TryFind(Key.Value "2") |> Option.defaultValue x.Draw
            Win2 = items.TryFind(Key.Value "3") |> Option.defaultValue x.Win2
        }
type Handicap = {
    Value : decimal
    Win1 : Betting.Outcome
    Win2 : Betting.Outcome
}
with
    static member MakeKey (value': decimal) =
        Key.Value (value'.ToString())
    member x.Key = Handicap.MakeKey (x.Value)
    member x.ItemValues () =
        [| x.Win1; x.Win2 |]
    member x.ItemIndexes () =
        [| Key.Value "2"; Key.Value "3" |]
    member x.Item = function
        | Key.Value "2" -> Some x.Win1
        | Key.Value "3" -> Some x.Win2
        | _ -> None
    member x.WithItems (items:Map<Key,_>) =
        {x with
            Win1 = items.TryFind(Key.Value "2") |> Option.defaultValue x.Win1
            Win2 = items.TryFind(Key.Value "3") |> Option.defaultValue x.Win2
        }
type Total = {
    Value : decimal
    Over : Betting.Outcome
    Under : Betting.Outcome
}
with
    static member MakeKey (value': decimal) =
        Key.Value (value'.ToString())
    member x.Key = Total.MakeKey (x.Value)
    member x.ItemValues () =
        [| x.Over; x.Under |]
    member x.ItemIndexes () =
        [| Key.Value "2"; Key.Value "3" |]
    member x.Item = function
        | Key.Value "2" -> Some x.Over
        | Key.Value "3" -> Some x.Under
        | _ -> None
    member x.WithItems (items:Map<Key,_>) =
        {x with
            Over = items.TryFind(Key.Value "2") |> Option.defaultValue x.Over
            Under = items.TryFind(Key.Value "3") |> Option.defaultValue x.Under
        }
type Score = {
    S1 : int
    S2 : int
}
with
    static member MakeKey (s1': int, s2': int) =
        Key.Items [Key.Value (s1'.ToString()); Key.Value (s2'.ToString())]
    member x.Key = Score.MakeKey (x.S1, x.S2)
type ScoreOutcome = {
    Score : Betting.Score
    Outcome : Betting.Outcome
}
type CorrectScore = {
    Scores : Betting.ScoreOutcome array
}
with
    member x.ItemValues () =
        [| yield! x.Scores |> Array.map (fun v -> v.Outcome) |]
    member x.ItemIndexes () =
        [| yield! x.Scores |> Array.map (fun v -> v.Score.Key) |]
    member x.TryFindItemInScores (key:Key) =
        x.Scores |> Array.tryFind (fun i -> i.Score.Key = key)
    member x.Item = function
        | TryFind x.TryFindItemInScores v -> Some v.Outcome
        | _ -> None
    member x.WithItems (items:Map<Key,_>) =
        {x with
            Scores = x.Scores |> Array.map (fun v -> items.TryFind v.Score.Key |> Option.map (fun i -> {v with Outcome = i}) |> Option.defaultValue v)
        }
type Market =
    | Unknown
    | Winner3Way of p1:Betting.Winner3Way
    | Handicap of p1:Betting.Handicap
    | Total of p1:Betting.Total
    | CorrectScore of p1:Betting.CorrectScore
with
    static member MakeUnknownKey () = Key.Value "0"
    static member MakeWinner3WayKey () = Key.Value "1"
    static member MakeHandicapKey (p1Key: Key) = Key.Items [Key.Value "2"; Key.Inner p1Key]
    static member MakeTotalKey (p1Key: Key) = Key.Items [Key.Value "3"; Key.Inner p1Key]
    static member MakeCorrectScoreKey () = Key.Value "4"
    member x.Key =
        match x with
        | Unknown -> Market.MakeUnknownKey ()
        | Winner3Way (p1') -> Market.MakeWinner3WayKey ()
        | Handicap (p1') -> Market.MakeHandicapKey (p1'.Key)
        | Total (p1') -> Market.MakeTotalKey (p1'.Key)
        | CorrectScore (p1') -> Market.MakeCorrectScoreKey ()
    member x.ItemValues () =
        match x with
        | Betting.Market.Unknown -> Array.empty
        | Betting.Market.Winner3Way (p1') -> p1'.ItemValues()
        | Betting.Market.Handicap (p1') -> p1'.ItemValues()
        | Betting.Market.Total (p1') -> p1'.ItemValues()
        | Betting.Market.CorrectScore (p1') -> p1'.ItemValues()
    member x.ItemIndexes () =
        match x with
        | Betting.Market.Unknown -> Array.empty
        | Betting.Market.Winner3Way (p1') -> p1'.ItemIndexes()
        | Betting.Market.Handicap (p1') -> p1'.ItemIndexes()
        | Betting.Market.Total (p1') -> p1'.ItemIndexes()
        | Betting.Market.CorrectScore (p1') -> p1'.ItemIndexes()
    member x.Item (key: Key) =
        match x with
        | Betting.Market.Unknown -> None
        | Betting.Market.Winner3Way (p1') -> p1'.Item key
        | Betting.Market.Handicap (p1') -> p1'.Item key
        | Betting.Market.Total (p1') -> p1'.Item key
        | Betting.Market.CorrectScore (p1') -> p1'.Item key
    member x.WithItems (items: Map<Key,_>) =
        match x with
        | Betting.Market.Unknown -> x
        | Betting.Market.Winner3Way (p1') -> Betting.Market.Winner3Way (p1'.WithItems items)
        | Betting.Market.Handicap (p1') -> Betting.Market.Handicap (p1'.WithItems items)
        | Betting.Market.Total (p1') -> Betting.Market.Total (p1'.WithItems items)
        | Betting.Market.CorrectScore (p1') -> Betting.Market.CorrectScore (p1'.WithItems items)
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
    Statistic : Betting.Statistic
    Period : Betting.Period
    Market : Betting.Market
    Status : Betting.Status
}
with
    static member MakeKey (statistic': Betting.Statistic, period': Betting.Period, marketKey: Key) =
        Key.Items [Key.Value ((int statistic').ToString()); Key.Value ((int period').ToString()); Key.Inner marketKey]
    member x.Key = MarketItem.MakeKey (x.Statistic, x.Period, x.Market.Key)
