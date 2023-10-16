namespace Test.Domain

open Protokeep.FsharpTypes

type ThreeWaysMarket = {
    Win1 : decimal
    Draw : decimal
    Win2 : decimal
}
with
    static member Default: Lazy<ThreeWaysMarket> =
        lazy {
            Win1 = 0m
            Draw = 0m
            Win2 = 0m
        }

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

type TwoWaysMarket = {
    Win1 : decimal
    Win2 : decimal
}
with
    static member Default: Lazy<TwoWaysMarket> =
        lazy {
            Win1 = 0m
            Win2 = 0m
        }

    member x.ItemValues () =
        [| x.Win1; x.Win2 |]

    member x.ItemIndexes () =
        [| Key.Value "1"; Key.Value "2" |]


    member x.Item = function
        | Key.Value "1" -> Some x.Win1
        | Key.Value "2" -> Some x.Win2
        | _ -> None

    member x.WithItems (items:Map<Key,_>) =
        {x with
            Win1 = items.TryFind(Key.Value "1") |> Option.defaultValue x.Win1
            Win2 = items.TryFind(Key.Value "2") |> Option.defaultValue x.Win2
        }

type HandicapMarket = {
    Value : decimal
    Win1 : decimal
    Win2 : decimal
}
with
    static member Default: Lazy<HandicapMarket> =
        lazy {
            Value = 0m
            Win1 = 0m
            Win2 = 0m
        }

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

type Market =
    | Unknown
    | ThreeWaysMarket of p1: ThreeWaysMarket
    | TwoWaysMarket of p1: TwoWaysMarket
    | HandicapMarket of p1: HandicapMarket

    static member Default: Lazy<Market> = lazy Market.Unknown

    static member MakeUnknownKey() = Key.Value "0"
    static member MakeThreeWaysMarketKey() = Key.Value "1"
    static member MakeTwoWaysMarketKey() = Key.Value "2"
    static member MakeHandicapMarketKey() = Key.Value "3"

    interface IEntity with
        member x.Key =
            match x with
            | Market.Unknown -> Market.MakeUnknownKey()
            | Market.ThreeWaysMarket(p1') -> Market.MakeThreeWaysMarketKey()
            | Market.TwoWaysMarket(p1') -> Market.MakeTwoWaysMarketKey()
            | Market.HandicapMarket(p1') -> Market.MakeHandicapMarketKey()

    member x.ItemValues () =
        match x with
        | Market.Unknown -> Array.empty
        | Market.ThreeWaysMarket(p1') -> p1'.ItemValues()
        | Market.TwoWaysMarket(p1') -> p1'.ItemValues()
        | Market.HandicapMarket(p1') -> p1'.ItemValues()
    member x.ItemIndexes () =
        match x with
        | Market.Unknown -> Array.empty
        | Market.ThreeWaysMarket(p1') -> p1'.ItemIndexes()
        | Market.TwoWaysMarket(p1') -> p1'.ItemIndexes()
        | Market.HandicapMarket(p1') -> p1'.ItemIndexes()
    member x.Item (key: Key) =
        match x with
        | Market.Unknown -> None
        | Market.ThreeWaysMarket(p1') -> p1'.Item key
        | Market.TwoWaysMarket(p1') -> p1'.Item key
        | Market.HandicapMarket(p1') -> p1'.Item key
    member x.WithItems (items: Map<Key,_>) =
        match x with
        | Market.Unknown -> x
        | Market.ThreeWaysMarket(p1') -> Market.ThreeWaysMarket (p1'.WithItems items)
        | Market.TwoWaysMarket(p1') -> Market.TwoWaysMarket (p1'.WithItems items)
        | Market.HandicapMarket(p1') -> Market.HandicapMarket (p1'.WithItems items)

type Score = {
    S1 : int
    S2 : int
}
with
    static member Default: Lazy<Score> =
        lazy {
            S1 = 0
            S2 = 0
        }

    static member MakeKey (s1': int, s2': int) =
        Key.Items [Key.Value (s1'.ToString()); Key.Value (s2'.ToString())]

    interface IEntity with
        member x.Key = Score.MakeKey (x.S1, x.S2)

type GoalDetails = {
    Score : Score
    Comment : string
}
with
    static member Default: Lazy<GoalDetails> =
        lazy {
            Score = Score.Default.Value
            Comment = ""
        }

type MatchProtocol = {
    Details : GoalDetails list
}
with
    static member Default: Lazy<MatchProtocol> =
        lazy {
            Details = List.empty
        }

    member x.ItemValues () =
        [| yield! x.Details |> Seq.map (fun v -> v.Comment) |]

    member x.ItemIndexes () =
        [| yield! x.Details |> Seq.map (fun v -> (v.Score :> IEntity).Key) |]

    member x.TryFindItemInDetails (key:Key) =
        x.Details |> Seq.tryFind (fun i -> (i.Score :> IEntity).Key = key)

    member x.Item = function
        | TryFind x.TryFindItemInDetails v -> Some v.Comment
        | _ -> None

    member x.WithItems (items:Map<Key,_>) =
        {x with
            Details = x.Details |> List.map (fun v -> items.TryFind (v.Score :> IEntity).Key |> Option.map (fun i -> {v with Comment = i}) |> Option.defaultValue v)
        }

