namespace Betting.Football

open Protokeep.FsharpTypes

type Market =
    | Unknown
    | Winner3Way of p1: Betting.Winner3Way
    | Handicap of p1: Betting.Handicap
    | Total of p1: Betting.Total
    | CorrectScore of p1: Betting.CorrectScore

    static member MakeUnknownKey () = Key.Value "0"
    static member MakeWinner3WayKey () = Key.Value "1"
    static member MakeHandicapKey (p1Key: Key) = Key.Items [Key.Value "2"; Key.Inner p1Key]
    static member MakeTotalKey (p1Key: Key) = Key.Items [Key.Value "3"; Key.Inner p1Key]
    static member MakeCorrectScoreKey () = Key.Value "4"
    member x.Key =
        match x with
        | Market.Unknown -> Market.MakeUnknownKey ()
        | Market.Winner3Way (p1') -> Market.MakeWinner3WayKey ()
        | Market.Handicap (p1') -> Market.MakeHandicapKey (p1'.Key)
        | Market.Total (p1') -> Market.MakeTotalKey (p1'.Key)
        | Market.CorrectScore (p1') -> Market.MakeCorrectScoreKey ()
    member x.ItemValues () =
        match x with
        | Market.Unknown -> Array.empty
        | Market.Winner3Way (p1') -> p1'.ItemValues()
        | Market.Handicap (p1') -> p1'.ItemValues()
        | Market.Total (p1') -> p1'.ItemValues()
        | Market.CorrectScore (p1') -> p1'.ItemValues()
    member x.ItemIndexes () =
        match x with
        | Market.Unknown -> Array.empty
        | Market.Winner3Way (p1') -> p1'.ItemIndexes()
        | Market.Handicap (p1') -> p1'.ItemIndexes()
        | Market.Total (p1') -> p1'.ItemIndexes()
        | Market.CorrectScore (p1') -> p1'.ItemIndexes()
    member x.Item (key: Key) =
        match x with
        | Market.Unknown -> None
        | Market.Winner3Way (p1') -> p1'.Item key
        | Market.Handicap (p1') -> p1'.Item key
        | Market.Total (p1') -> p1'.Item key
        | Market.CorrectScore (p1') -> p1'.Item key
    member x.WithItems (items: Map<Key,_>) =
        match x with
        | Market.Unknown -> x
        | Market.Winner3Way (p1') -> Market.Winner3Way (p1'.WithItems items)
        | Market.Handicap (p1') -> Market.Handicap (p1'.WithItems items)
        | Market.Total (p1') -> Market.Total (p1'.WithItems items)
        | Market.CorrectScore (p1') -> Market.CorrectScore (p1'.WithItems items)

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
    Statistic : Statistic
    Period : Period
    Market : Market
    Status : Status
    Version : int
}
with
    static member Default: Lazy<MarketItem> =
        lazy {
            Statistic = Statistic.Unknown
            Period = Period.Unknown
            Market = Market.Unknown
            Status = Status.Unknown
            Version = 0
        }
    static member MakeKey (statistic': Statistic, period': Period, marketKey: Key) =
        Key.Items [Key.Value ((int statistic').ToString()); Key.Value ((int period').ToString()); Key.Inner marketKey]
    member x.Key = MarketItem.MakeKey (x.Statistic, x.Period, x.Market.Key)

