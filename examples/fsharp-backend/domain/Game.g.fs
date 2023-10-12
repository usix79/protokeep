namespace Example.GameDomain

open Protokeep.FsharpTypes

type SessionOwner =
    | Unknown
    | Guest
    | Registered of playerId: System.Guid

    static member MakeUnknownKey() = Key.Value "0"
    static member MakeGuestKey() = Key.Value "1"
    static member MakeRegisteredKey() = Key.Value "2"

    interface IEntity with
        member x.Key =
            match x with
            | SessionOwner.Unknown -> SessionOwner.MakeUnknownKey()
            | SessionOwner.Guest -> SessionOwner.MakeGuestKey()
            | SessionOwner.Registered(playerId') -> SessionOwner.MakeRegisteredKey()


type Connection = {
    Id : string
}
with
    static member Default: Lazy<Connection> =
        lazy {
            Id = ""
        }

type Session = {
    Id : System.Guid
    Owner : SessionOwner
    CurrentConnection : Connection voption
    CurrentMatch : System.Guid voption
    ExpiredAt : System.DateTime
    mutable Version : int
}
with
    static member Default: Lazy<Session> =
        lazy {
            Id = System.Guid.Empty
            Owner = SessionOwner.Unknown
            CurrentConnection = ValueNone
            CurrentMatch = ValueNone
            ExpiredAt = System.DateTime.MinValue
            Version = 0
        }

    static member MakeKey (id': System.Guid) =
        Key.Value (id'.ToString())

    interface IEntity with
        member x.Key = Session.MakeKey (x.Id)

    interface IVersioned with
        member x.Version
            with get () = x.Version
            and set v = x.Version <- v

type Side =
    | Unknown = 0
    | Player1 = 1
    | Player2 = 2

type GameStatus =
    | Unknown
    | InProgress of turn: int16
    | Finnished of winner: Side * turn: int16
    | Terminated

    static member MakeUnknownKey() = Key.Value "0"
    static member MakeInProgressKey() = Key.Value "1"
    static member MakeFinnishedKey() = Key.Value "2"
    static member MakeTerminatedKey() = Key.Value "3"

    interface IEntity with
        member x.Key =
            match x with
            | GameStatus.Unknown -> GameStatus.MakeUnknownKey()
            | GameStatus.InProgress(turn') -> GameStatus.MakeInProgressKey()
            | GameStatus.Finnished(winner', turn') -> GameStatus.MakeFinnishedKey()
            | GameStatus.Terminated -> GameStatus.MakeTerminatedKey()


[<Struct>]
type Location = {
    X : sbyte
    Y : sbyte
}
with
    static member Default: Lazy<Location> =
        lazy {
            X = 0y
            Y = 0y
        }

type Unit = {
    Name : string
    Health : sbyte
}
with
    static member Default: Lazy<Unit> =
        lazy {
            Name = ""
            Health = 0y
        }

type Game = {
    Id : System.Guid
    Player : sbyte
    Status : GameStatus
    Board : Map<Location,Unit>
    LastChange : System.DateTime
    mutable Version : int
}
with
    static member Default: Lazy<Game> =
        lazy {
            Id = System.Guid.Empty
            Player = 0y
            Status = GameStatus.Unknown
            Board = Map.empty
            LastChange = System.DateTime.MinValue
            Version = 0
        }

    static member MakeKey (id': System.Guid) =
        Key.Value (id'.ToString())

    interface IEntity with
        member x.Key = Game.MakeKey (x.Id)

    interface IVersioned with
        member x.Version
            with get () = x.Version
            and set v = x.Version <- v

[<Struct>]
type Action =
    | Unknown
    | EndOfTurn
    | Drop of dropPoint: Location
    | Move of fromPoint: Location * toPoint: Location

    static member MakeUnknownKey() = Key.Value "0"
    static member MakeEndOfTurnKey() = Key.Value "1"
    static member MakeDropKey() = Key.Value "2"
    static member MakeMoveKey() = Key.Value "3"

    interface IEntity with
        member x.Key =
            match x with
            | Action.Unknown -> Action.MakeUnknownKey()
            | Action.EndOfTurn -> Action.MakeEndOfTurnKey()
            | Action.Drop(dropPoint') -> Action.MakeDropKey()
            | Action.Move(fromPoint', toPoint') -> Action.MakeMoveKey()


type Request = {
    Game : Game
    Action : Action
}
with
    static member Default: Lazy<Request> =
        lazy {
            Game = Game.Default.Value
            Action = Action.Unknown
        }

type Response =
    | Unknown
    | Ok of game: Game * possibleActions: Action list
    | Fail of errors: string list

    static member MakeUnknownKey() = Key.Value "0"
    static member MakeOkKey() = Key.Value "1"
    static member MakeFailKey() = Key.Value "2"

    interface IEntity with
        member x.Key =
            match x with
            | Response.Unknown -> Response.MakeUnknownKey()
            | Response.Ok(game', possibleActions') -> Response.MakeOkKey()
            | Response.Fail(errors') -> Response.MakeFailKey()


