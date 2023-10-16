namespace Test.Domain

open Protokeep.FsharpTypes

type Incident =
    | Unknown
    | SwitchedOff
    | MissedTurns of count: int
    | Delayes of from: System.DateTime * to: System.DateTime
    | Root of p1: Incident list
    | Noise of p1: Map<string,string>

    static member Default: Lazy<Incident> = lazy Incident.Unknown

    static member MakeUnknownKey() = Key.Value "0"
    static member MakeSwitchedOffKey() = Key.Value "1"
    static member MakeMissedTurnsKey() = Key.Value "2"
    static member MakeDelayesKey() = Key.Value "3"
    static member MakeRootKey() = Key.Value "4"
    static member MakeNoiseKey() = Key.Value "5"

    interface IEntity with
        member x.Key =
            match x with
            | Incident.Unknown -> Incident.MakeUnknownKey()
            | Incident.SwitchedOff -> Incident.MakeSwitchedOffKey()
            | Incident.MissedTurns(count') -> Incident.MakeMissedTurnsKey()
            | Incident.Delayes(from', to') -> Incident.MakeDelayesKey()
            | Incident.Root(p1') -> Incident.MakeRootKey()
            | Incident.Noise(p1') -> Incident.MakeNoiseKey()


