module rec Betting
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
type Total = {
    Value : decimal
    Over : Betting.Outcome
    Under : Betting.Outcome
}
type Score = {
    S1 : int
    S2 : int
}
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
