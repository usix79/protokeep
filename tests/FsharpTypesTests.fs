module FsharpTypesTests

open FSharp.Reflection
open Xunit
open Protogen.Types
open Protogen

type TestData() =
  static member MyTestData =
    [
        ("""
module Domain
enum TrafficLight =
    | Red
    | Yellow
    | Green ""","""
namespace Domain
open Protogen.FsharpTypes
type TrafficLight =
    | Unknown = 0
    | Red = 1
    | Yellow = 2
    | Green = 3
"""    );
    ("""
module Domain
enum TrafficLight =
    | Red
    | Yellow
    | Green
record Crossroad = {
    Id: int
    LongId: long
    AltId: guid
    Street1: string
    Street2: string
    IsMonitored: bool
    Xpos: float
    Ypos: double
    Ratio: decimal(2)
    LastChecked: timestamp
    ServiceInterval: duration
    CurrentLight: TrafficLight
    Nickname: string option
    Img: bytes
    Notes: string array
    Siblings: int list
    Props: string map
}""","""
namespace Domain
open Protogen.FsharpTypes
type TrafficLight =
    | Unknown = 0
    | Red = 1
    | Yellow = 2
    | Green = 3
type Crossroad = {
    Id : int
    LongId : int64
    AltId : System.Guid
    Street1 : string
    Street2 : string
    IsMonitored : bool
    Xpos : float32
    Ypos : float
    Ratio : decimal
    LastChecked : System.DateTimeOffset
    ServiceInterval : System.TimeSpan
    CurrentLight : TrafficLight
    Nickname : string option
    Img : byte array
    Notes : string array
    Siblings : int list
    Props : Map<string,string>
}""");
    ("""
module Domain

enum TrafficLight = Red | Yellow | Green

union LightStatus =
    | Normal
    | Warning of errorsCount:int
    | OutOfOrder of since:timestamp

record Crossroad = {
    Id: int
    Street1: string
    Street2: string
    Light: TrafficLight
    LightStatus: LightStatus
}""", """
namespace Domain
open Protogen.FsharpTypes
type TrafficLight =
    | Unknown = 0
    | Red = 1
    | Yellow = 2
    | Green = 3
type LightStatus =
    | Unknown
    | Normal
    | Warning of errorsCount:int
    | OutOfOrder of since:System.DateTimeOffset
with
    static member MakeUnknownKey () = Key.Value "0"
    static member MakeNormalKey () = Key.Value "1"
    static member MakeWarningKey () = Key.Value "2"
    static member MakeOutOfOrderKey () = Key.Value "3"
    member x.Key =
        match x with
        | LightStatus.Unknown -> LightStatus.MakeUnknownKey ()
        | LightStatus.Normal -> LightStatus.MakeNormalKey ()
        | LightStatus.Warning (errorsCount') -> LightStatus.MakeWarningKey ()
        | LightStatus.OutOfOrder (since') -> LightStatus.MakeOutOfOrderKey ()
type Crossroad = {
    Id : int
    Street1 : string
    Street2 : string
    Light : TrafficLight
    LightStatus : LightStatus
}
"""); ("""
module Domain
record Score = {
    S1 : int key
    S2 : int
}
""", """
namespace Domain
open Protogen.FsharpTypes
type Score = {
    S1 : int
    S2 : int
}
with
    static member MakeKey (s1': int) =
        Key.Value (s1'.ToString())
    member x.Key = Score.MakeKey (x.S1)
"""); ("""
module Domain
record Score = {
    S1 : int key
    S2 : int key
}
""", """
namespace Domain
open Protogen.FsharpTypes
type Score = {
    S1 : int
    S2 : int
}
with
    static member MakeKey (s1': int, s2': int) =
        Key.Items [Key.Value (s1'.ToString()); Key.Value (s2'.ToString())]
    member x.Key = Score.MakeKey (x.S1, x.S2)
""");("""
module Domain
enum TrafficLight = Red | Yellow | Green
record Crossroad = {
    Id: int key
    Light: TrafficLight key
    Street1: string
    Street2: string
}
""", """
namespace Domain
open Protogen.FsharpTypes
type TrafficLight =
    | Unknown = 0
    | Red = 1
    | Yellow = 2
    | Green = 3
type Crossroad = {
    Id : int
    Light : TrafficLight
    Street1 : string
    Street2 : string
}
with
    static member MakeKey (id': int, light': TrafficLight) =
        Key.Items [Key.Value (id'.ToString()); Key.Value ((int light').ToString())]
    member x.Key = Crossroad.MakeKey (x.Id, x.Light)
"""); ("""
module Domain
record Score = {
    S1 : int key
    S2 : int key
}
record Incident = {
    Score: Score key
    Details: string
}
""", """
namespace Domain
open Protogen.FsharpTypes
type Score = {
    S1 : int
    S2 : int
}
with
    static member MakeKey (s1': int, s2': int) =
        Key.Items [Key.Value (s1'.ToString()); Key.Value (s2'.ToString())]
    member x.Key = Score.MakeKey (x.S1, x.S2)
type Incident = {
    Score : Score
    Details : string
}
with
    static member MakeKey (scoreKey: Key) =
        Key.Inner scoreKey
    member x.Key = Incident.MakeKey (x.Score.Key)
""");("""
module Domain
record Score = {
    S1 : int key
    S2 : int key
}
union Incident =
    | MatchStarted
    | TeamScored of int key
    | MatchFinished of Score key
""", """
namespace Domain
open Protogen.FsharpTypes
type Score = {
    S1 : int
    S2 : int
}
with
    static member MakeKey (s1': int, s2': int) =
        Key.Items [Key.Value (s1'.ToString()); Key.Value (s2'.ToString())]
    member x.Key = Score.MakeKey (x.S1, x.S2)
type Incident =
    | Unknown
    | MatchStarted
    | TeamScored of p1:int
    | MatchFinished of p1:Score
with
    static member MakeUnknownKey () = Key.Value "0"
    static member MakeMatchStartedKey () = Key.Value "1"
    static member MakeTeamScoredKey (p1': int) = Key.Items [Key.Value "2"; Key.Value (p1'.ToString())]
    static member MakeMatchFinishedKey (p1Key: Key) = Key.Items [Key.Value "3"; Key.Inner p1Key]
    member x.Key =
        match x with
        | Incident.Unknown -> Incident.MakeUnknownKey ()
        | Incident.MatchStarted -> Incident.MakeMatchStartedKey ()
        | Incident.TeamScored (p1') -> Incident.MakeTeamScoredKey (p1')
        | Incident.MatchFinished (p1') -> Incident.MakeMatchFinishedKey (p1'.Key)
""");("""
module Domain
record Score = {
    S1 : int idx
    S2 : int idx
}
""", """
namespace Domain
open Protogen.FsharpTypes
type Score = {
    S1 : int
    S2 : int
}
with
    member x.ItemValues () =
        [| x.S1; x.S2 |]
    member x.ItemIndexes () =
        [| Key.Value "1"; Key.Value "2" |]
    member x.Item = function
        | Key.Value "1" -> Some x.S1
        | Key.Value "2" -> Some x.S2
        | _ -> None
    member x.WithItems (items:Map<Key,_>) =
        {x with
            S1 = items.TryFind(Key.Value "1") |> Option.defaultValue x.S1
            S2 = items.TryFind(Key.Value "2") |> Option.defaultValue x.S2
        }
""");("""
module Domain
record Triple = {
    Comment1: string idx
    Comment2: string idx
    Comment3: string idx
}
record Score = {
    S1 : int key
    S2 : int key
}
record ScoreOutcome = {
    Score: Score
    Comment: string
}
record CorrectScore = {
    Scores : ScoreOutcome list idx[.Score => .Comment]
}
union Market =
    | Triple of Triple
    | Multiple of CorrectScore
""", """
namespace Domain
open Protogen.FsharpTypes
type Triple = {
    Comment1 : string
    Comment2 : string
    Comment3 : string
}
with
    member x.ItemValues () =
        [| x.Comment1; x.Comment2; x.Comment3 |]
    member x.ItemIndexes () =
        [| Key.Value "1"; Key.Value "2"; Key.Value "3" |]
    member x.Item = function
        | Key.Value "1" -> Some x.Comment1
        | Key.Value "2" -> Some x.Comment2
        | Key.Value "3" -> Some x.Comment3
        | _ -> None
    member x.WithItems (items:Map<Key,_>) =
        {x with
            Comment1 = items.TryFind(Key.Value "1") |> Option.defaultValue x.Comment1
            Comment2 = items.TryFind(Key.Value "2") |> Option.defaultValue x.Comment2
            Comment3 = items.TryFind(Key.Value "3") |> Option.defaultValue x.Comment3
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
    Comment : string
}
type CorrectScore = {
    Scores : ScoreOutcome list
}
with
    member x.ItemValues () =
        [| yield! x.Scores |> Seq.map (fun v -> v.Comment) |]
    member x.ItemIndexes () =
        [| yield! x.Scores |> Seq.map (fun v -> v.Score.Key) |]
    member x.TryFindItemInScores (key:Key) =
        x.Scores |> Seq.tryFind (fun i -> i.Score.Key = key)
    member x.Item = function
        | TryFind x.TryFindItemInScores v -> Some v.Comment
        | _ -> None
    member x.WithItems (items:Map<Key,_>) =
        {x with
            Scores = x.Scores |> List.map (fun v -> items.TryFind v.Score.Key |> Option.map (fun i -> {v with Comment = i}) |> Option.defaultValue v)
        }
type Market =
    | Unknown
    | Triple of p1:Triple
    | Multiple of p1:CorrectScore
with
    static member MakeUnknownKey () = Key.Value "0"
    static member MakeTripleKey () = Key.Value "1"
    static member MakeMultipleKey () = Key.Value "2"
    member x.Key =
        match x with
        | Market.Unknown -> Market.MakeUnknownKey ()
        | Market.Triple (p1') -> Market.MakeTripleKey ()
        | Market.Multiple (p1') -> Market.MakeMultipleKey ()
    member x.ItemValues () =
        match x with
        | Market.Unknown -> Array.empty
        | Market.Triple (p1') -> p1'.ItemValues()
        | Market.Multiple (p1') -> p1'.ItemValues()
    member x.ItemIndexes () =
        match x with
        | Market.Unknown -> Array.empty
        | Market.Triple (p1') -> p1'.ItemIndexes()
        | Market.Multiple (p1') -> p1'.ItemIndexes()
    member x.Item (key: Key) =
        match x with
        | Market.Unknown -> None
        | Market.Triple (p1') -> p1'.Item key
        | Market.Multiple (p1') -> p1'.Item key
    member x.WithItems (items: Map<Key,_>) =
        match x with
        | Market.Unknown -> x
        | Market.Triple (p1') -> Market.Triple (p1'.WithItems items)
        | Market.Multiple (p1') -> Market.Multiple (p1'.WithItems items)
""");
    ] |> Seq.map FSharpValue.GetTupleFields

[<Theory; MemberData("MyTestData", MemberType=typeof<TestData>)>]
let testAllCases (input, expectedOutput:string) =
    Parsers.parsePgenDoc input
    |> Result.bind(fun module' ->
        Types.resolveReferences module' []
        |> Result.mapError(fun error -> failwithf "%A" error)
        |> Result.bind (fun (module',typesCache) ->
            Types.lock module' (LocksCollection []) typesCache
            |> Result.map(fun locks ->
                let outputText = FsharpTypesCmd.gen module' (LocksCollection locks) typesCache
                Assert.Equal(expectedOutput.Trim(), outputText.Trim()))
            |> Result.mapError(fun error -> failwithf "%A" error)))
    |> Result.mapError(fun error -> failwithf "%A" error)