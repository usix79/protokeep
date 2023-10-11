namespace Protokeep.FsharpProto

type ConvertExampleGameDomain() =

    static member FromProtobuf(x: ProtoClasses.Example.GameDomain.Side) : Example.GameDomain.Side =
        enum<Example.GameDomain.Side> (int x)

    static member ToProtobuf(x: Example.GameDomain.Side) : ProtoClasses.Example.GameDomain.Side =
        enum<ProtoClasses.Example.GameDomain.Side> (int x)

    static member FromProtobuf(x: ProtoClasses.Example.GameDomain.GameStatus) : Example.GameDomain.GameStatus =
        match x.UnionCase with
        | ProtoClasses.Example.GameDomain.GameStatus.UnionOneofCase.InProgress -> Example.GameDomain.GameStatus.InProgress(x.InProgress |> int16)
        | ProtoClasses.Example.GameDomain.GameStatus.UnionOneofCase.Finnished -> x.Finnished |> ConvertExampleGameDomain.FromProtobuf
        | ProtoClasses.Example.GameDomain.GameStatus.UnionOneofCase.Terminated -> Example.GameDomain.GameStatus.Terminated
        | _ -> Example.GameDomain.GameStatus.Unknown
    static member ToProtobuf(x: Example.GameDomain.GameStatus) : ProtoClasses.Example.GameDomain.GameStatus =
        let y = ProtoClasses.Example.GameDomain.GameStatus()
        match x with
        | Example.GameDomain.GameStatus.InProgress (turn) ->
            y.InProgress <- turn |> int
        | Example.GameDomain.GameStatus.Finnished (winner,turn) -> y.Finnished <- ConvertExampleGameDomain.GameStatusCaseFinnishedToProtobuf(winner,turn)
        | Example.GameDomain.GameStatus.Terminated -> y.Terminated <- true
        | Example.GameDomain.GameStatus.Unknown -> ()
        y
    static member FromProtobuf (x:ProtoClasses.Example.GameDomain.GameStatus__Finnished) =
        Example.GameDomain.GameStatus.Finnished
            ((x.Winner |> ConvertExampleGameDomain.FromProtobuf),(x.Turn |> int16))

    static member GameStatusCaseFinnishedToProtobuf (winner,turn) : ProtoClasses.Example.GameDomain.GameStatus__Finnished =
        let y = ProtoClasses.Example.GameDomain.GameStatus__Finnished()
        y.Winner <- winner |> ConvertExampleGameDomain.ToProtobuf
        y.Turn <- turn |> int
        y


    static member FromProtobuf(x: ProtoClasses.Example.GameDomain.Location) : Example.GameDomain.Location =
        {
            X = x.X |> sbyte
            Y = x.Y |> sbyte
        }

    static member ToProtobuf(x: Example.GameDomain.Location) : ProtoClasses.Example.GameDomain.Location =
        let y = ProtoClasses.Example.GameDomain.Location()
        y.X <- x.X |> int
        y.Y <- x.Y |> int
        y

    static member FromProtobuf(x: ProtoClasses.Example.GameDomain.Unit) : Example.GameDomain.Unit =
        {
            Name = x.Name
            Health = x.Health |> sbyte
        }

    static member ToProtobuf(x: Example.GameDomain.Unit) : ProtoClasses.Example.GameDomain.Unit =
        let y = ProtoClasses.Example.GameDomain.Unit()
        y.Name <- x.Name
        y.Health <- x.Health |> int
        y

    static member FromProtobuf(x: ProtoClasses.Example.GameDomain.Game) : Example.GameDomain.Game =
        {
            Id = x.Id |> fun v -> System.Guid(v.ToByteArray())
            Player = x.Player |> sbyte
            Status = x.Status |> ConvertExampleGameDomain.FromProtobuf
            Board = x.Board |> Seq.map(fun x -> x.Key |> ConvertExampleGameDomain.FromProtobuf, x.Value |> ConvertExampleGameDomain.FromProtobuf) |> Map.ofSeq
            LastChange = x.LastChange |> fun v -> v.ToDateTime()
            Version = x.Version
        }

    static member ToProtobuf(x: Example.GameDomain.Game) : ProtoClasses.Example.GameDomain.Game =
        let y = ProtoClasses.Example.GameDomain.Game()
        y.Id <- x.Id |> fun v -> Google.Protobuf.ByteString.CopyFrom(v.ToByteArray())
        y.Player <- x.Player |> int
        y.Status <- x.Status |> ConvertExampleGameDomain.ToProtobuf
        for pair in x.Board do
            let protoPair = ProtoClasses.Example.GameDomain.LocationUnitPair ()
            protoPair.Key <- pair.Key |> ConvertExampleGameDomain.ToProtobuf
            protoPair.Value <- pair.Value |> ConvertExampleGameDomain.ToProtobuf
            y.Board.Add(protoPair)
        y.LastChange <- x.LastChange |> Google.Protobuf.WellKnownTypes.Timestamp.FromDateTime
        y.Version <- x.Version
        y

    static member FromProtobuf(x: ProtoClasses.Example.GameDomain.Action) : Example.GameDomain.Action =
        match x.UnionCase with
        | ProtoClasses.Example.GameDomain.Action.UnionOneofCase.EndOfTurn -> Example.GameDomain.Action.EndOfTurn
        | ProtoClasses.Example.GameDomain.Action.UnionOneofCase.Drop -> Example.GameDomain.Action.Drop(x.Drop |> ConvertExampleGameDomain.FromProtobuf)
        | ProtoClasses.Example.GameDomain.Action.UnionOneofCase.Move -> x.Move |> ConvertExampleGameDomain.FromProtobuf
        | _ -> Example.GameDomain.Action.Unknown
    static member ToProtobuf(x: Example.GameDomain.Action) : ProtoClasses.Example.GameDomain.Action =
        let y = ProtoClasses.Example.GameDomain.Action()
        match x with
        | Example.GameDomain.Action.EndOfTurn -> y.EndOfTurn <- true
        | Example.GameDomain.Action.Drop (dropPoint) ->
            y.Drop <- dropPoint |> ConvertExampleGameDomain.ToProtobuf
        | Example.GameDomain.Action.Move (fromPoint,toPoint) -> y.Move <- ConvertExampleGameDomain.ActionCaseMoveToProtobuf(fromPoint,toPoint)
        | Example.GameDomain.Action.Unknown -> ()
        y
    static member FromProtobuf (x:ProtoClasses.Example.GameDomain.Action__Move) =
        Example.GameDomain.Action.Move
            ((x.FromPoint |> ConvertExampleGameDomain.FromProtobuf),(x.ToPoint |> ConvertExampleGameDomain.FromProtobuf))

    static member ActionCaseMoveToProtobuf (fromPoint,toPoint) : ProtoClasses.Example.GameDomain.Action__Move =
        let y = ProtoClasses.Example.GameDomain.Action__Move()
        y.FromPoint <- fromPoint |> ConvertExampleGameDomain.ToProtobuf
        y.ToPoint <- toPoint |> ConvertExampleGameDomain.ToProtobuf
        y


    static member FromProtobuf(x: ProtoClasses.Example.GameDomain.Request) : Example.GameDomain.Request =
        {
            Game = x.Game |> ConvertExampleGameDomain.FromProtobuf
            Action = x.Action |> ConvertExampleGameDomain.FromProtobuf
        }

    static member ToProtobuf(x: Example.GameDomain.Request) : ProtoClasses.Example.GameDomain.Request =
        let y = ProtoClasses.Example.GameDomain.Request()
        y.Game <- x.Game |> ConvertExampleGameDomain.ToProtobuf
        y.Action <- x.Action |> ConvertExampleGameDomain.ToProtobuf
        y

    static member FromProtobuf(x: ProtoClasses.Example.GameDomain.Response) : Example.GameDomain.Response =
        match x.UnionCase with
        | ProtoClasses.Example.GameDomain.Response.UnionOneofCase.Ok -> x.Ok |> ConvertExampleGameDomain.FromProtobuf
        | ProtoClasses.Example.GameDomain.Response.UnionOneofCase.Fail -> x.Fail |> ConvertExampleGameDomain.FromProtobuf
        | _ -> Example.GameDomain.Response.Unknown
    static member ToProtobuf(x: Example.GameDomain.Response) : ProtoClasses.Example.GameDomain.Response =
        let y = ProtoClasses.Example.GameDomain.Response()
        match x with
        | Example.GameDomain.Response.Ok (game,possibleActions) -> y.Ok <- ConvertExampleGameDomain.ResponseCaseOkToProtobuf(game,possibleActions)
        | Example.GameDomain.Response.Fail (errors) -> y.Fail <- ConvertExampleGameDomain.ResponseCaseFailToProtobuf(errors)
        | Example.GameDomain.Response.Unknown -> ()
        y
    static member FromProtobuf (x:ProtoClasses.Example.GameDomain.Response__Ok) =
        Example.GameDomain.Response.Ok
            ((x.Game |> ConvertExampleGameDomain.FromProtobuf),(x.PossibleActions |> Seq.map(ConvertExampleGameDomain.FromProtobuf) |> List.ofSeq))

    static member ResponseCaseOkToProtobuf (game,possibleActions) : ProtoClasses.Example.GameDomain.Response__Ok =
        let y = ProtoClasses.Example.GameDomain.Response__Ok()
        y.Game <- game |> ConvertExampleGameDomain.ToProtobuf
        y.PossibleActions.AddRange(possibleActions |> Seq.map(ConvertExampleGameDomain.ToProtobuf))
        y

    static member FromProtobuf (x:ProtoClasses.Example.GameDomain.Response__Fail) =
        Example.GameDomain.Response.Fail
            ((x.Errors |> List.ofSeq))

    static member ResponseCaseFailToProtobuf (errors) : ProtoClasses.Example.GameDomain.Response__Fail =
        let y = ProtoClasses.Example.GameDomain.Response__Fail()
        y.Errors.AddRange(errors)
        y


