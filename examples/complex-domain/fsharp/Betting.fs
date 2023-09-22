namespace Betting
open Protokeep.FsharpTypes
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
    | Resulted of result:OutcomeResult
with
    static member MakeUnknownKey () = Key.Value "0"
    static member MakeEmptyKey () = Key.Value "1"
    static member MakePricedKey () = Key.Value "2"
    static member MakePricedWithProbKey () = Key.Value "3"
    static member MakeResultedKey () = Key.Value "4"
    member x.Key =
        match x with
        | Outcome.Unknown -> Outcome.MakeUnknownKey ()
        | Outcome.Empty -> Outcome.MakeEmptyKey ()
        | Outcome.Priced (price') -> Outcome.MakePricedKey ()
        | Outcome.PricedWithProb (price', prob') -> Outcome.MakePricedWithProbKey ()
        | Outcome.Resulted (result') -> Outcome.MakeResultedKey ()
type Winner3Way = {
    Win1 : Outcome
    Draw : Outcome
    Win2 : Outcome
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
    Win1 : Outcome
    Win2 : Outcome
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
    Over : Outcome
    Under : Outcome
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
    Score : Score
    Outcome : Outcome
}
type CorrectScore = {
    Scores : ScoreOutcome list
}
with
    member x.ItemValues () =
        [| yield! x.Scores |> Seq.map (fun v -> v.Outcome) |]
    member x.ItemIndexes () =
        [| yield! x.Scores |> Seq.map (fun v -> v.Score.Key) |]
    member x.TryFindItemInScores (key:Key) =
        x.Scores |> Seq.tryFind (fun i -> i.Score.Key = key)
    member x.Item = function
        | TryFind x.TryFindItemInScores v -> Some v.Outcome
        | _ -> None
    member x.WithItems (items:Map<Key,_>) =
        {x with
            Scores = x.Scores |> List.map (fun v -> items.TryFind v.Score.Key |> Option.map (fun i -> {v with Outcome = i}) |> Option.defaultValue v)
        }
