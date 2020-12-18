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
type Winner3Way = {
    Win1 : Betting.Outcome
    Draw : Betting.Outcome
    Win2 : Betting.Outcome
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
type Total = {
    Value : decimal
    Over : Betting.Outcome
    Under : Betting.Outcome
}
with
    static member MakeKey (value': decimal) =
        Key.Value (value'.ToString())
    member x.Key = Total.MakeKey (x.Value)
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
type Market =
    | Unknown
    | Winner3Way of p1:Betting.Winner3Way
    | Handicap of p1:Betting.Handicap
    | Total of p1:Betting.Total
    | CorrectScore of p1:Betting.CorrectScore
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
