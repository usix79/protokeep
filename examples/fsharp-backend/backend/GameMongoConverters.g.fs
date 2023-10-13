namespace Protokeep.FsharpMongo

open MongoDB.Bson
open MongoDB.Bson.IO
open MongoDB.Bson.Serialization
open MongoDB.Bson.Serialization.Serializers
open Protokeep

type ConvertExampleGameDomain() =
    static member SessionOwnerToBson(writer: IBsonWriter, x: Example.GameDomain.SessionOwner) =
        writer.WriteStartDocument()
        match x with
        | Example.GameDomain.SessionOwner.Guest ->
            writer.WriteName("Guest")
            writer.WriteBoolean(true)
        | Example.GameDomain.SessionOwner.Registered (playerId) ->
            writer.WriteName("Registered")
            FsharpMongoHelpers.writeGuid(writer, playerId)
        | _ ->
            writer.WriteName("Unknown")
            writer.WriteBoolean(true)
        writer.WriteEndDocument()
    static member SessionOwnerFromBson (reader: IBsonReader): Example.GameDomain.SessionOwner =
        let mutable y = Example.GameDomain.SessionOwner.Unknown
        reader.ReadStartDocument()
        match reader.ReadName() with
                | "Guest" ->
                    match FsharpMongoHelpers.readBoolean reader with
                    | ValueSome true -> y <- Example.GameDomain.SessionOwner.Guest
                    | _ -> ()
                | "Registered" ->
                    let mutable _playerId = System.Guid.Empty
                    match FsharpMongoHelpers.readGuid reader with
                    | ValueSome v -> _playerId <- v
                    | ValueNone -> ()
                    y <- _playerId |> Example.GameDomain.SessionOwner.Registered
                | _ -> ()
        reader.ReadEndDocument()
        y
    static member ConnectionToBson(writer: IBsonWriter, x: Example.GameDomain.Connection) =
        writer.WriteStartDocument()
        writer.WriteName("Id")
        writer.WriteString(x.Id)
        writer.WriteEndDocument()

    static member ConnectionFromBson(reader: IBsonReader): Example.GameDomain.Connection =
        let mutable vId = ""
        reader.ReadStartDocument()
        while reader.State <> BsonReaderState.EndOfDocument do
            match reader.State with
            | BsonReaderState.Type -> reader.ReadBsonType() |> ignore
            | BsonReaderState.Name ->
                match reader.ReadName() with
                | "Id" ->
                    match FsharpMongoHelpers.readString reader with
                    | ValueSome v -> vId <- v
                    | ValueNone -> ()
                | _ -> reader.SkipValue()
            | _ -> printfn "Unexpected state: %A" reader.State
        reader.ReadEndDocument()
        {
            Id = vId
        }

    static member SessionToBson(writer: IBsonWriter, x: Example.GameDomain.Session, ?asEntity: bool) =
        writer.WriteStartDocument()
        if asEntity.IsSome then
            FsharpMongoHelpers.writeId (writer, x)
        writer.WriteName("Id")
        FsharpMongoHelpers.writeGuid(writer, x.Id)
        writer.WriteName("Owner")
        ConvertExampleGameDomain.SessionOwnerToBson(writer, x.Owner)
        match x.CurrentConnection with
        | ValueSome v ->
            writer.WriteName("CurrentConnectionValue")
            ConvertExampleGameDomain.ConnectionToBson(writer, v)
        | ValueNone -> ()
        match x.CurrentMatch with
        | ValueSome v ->
            writer.WriteName("CurrentMatchValue")
            FsharpMongoHelpers.writeGuid(writer, v)
        | ValueNone -> ()
        writer.WriteName("ExpiredAt")
        writer.WriteDateTime(FsharpMongoHelpers.fromDateTime x.ExpiredAt)
        writer.WriteName("Version")
        writer.WriteInt32(x.Version)
        writer.WriteEndDocument()

    static member SessionFromBson(reader: IBsonReader): Example.GameDomain.Session =
        let mutable vId = System.Guid.Empty
        let mutable vOwner = Example.GameDomain.SessionOwner.Unknown
        let mutable vCurrentConnection = ValueNone
        let mutable vCurrentMatch = ValueNone
        let mutable vExpiredAt = System.DateTime.MinValue
        let mutable vVersion = 0
        reader.ReadStartDocument()
        while reader.State <> BsonReaderState.EndOfDocument do
            match reader.State with
            | BsonReaderState.Type -> reader.ReadBsonType() |> ignore
            | BsonReaderState.Name ->
                match reader.ReadName() with
                | "Id" ->
                    match FsharpMongoHelpers.readGuid reader with
                    | ValueSome v -> vId <- v
                    | ValueNone -> ()
                | "Owner" ->
                    match ConvertExampleGameDomain.SessionOwnerFromBson(reader) |> ValueSome with
                    | ValueSome v -> vOwner <- v
                    | ValueNone -> ()
                | "CurrentConnectionValue" ->
                    match ConvertExampleGameDomain.ConnectionFromBson(reader) |> ValueSome |> ValueOption.map ValueSome with
                    | ValueSome v -> vCurrentConnection <- v
                    | ValueNone -> ()
                | "CurrentMatchValue" ->
                    match FsharpMongoHelpers.readGuid reader |> ValueOption.map ValueSome with
                    | ValueSome v -> vCurrentMatch <- v
                    | ValueNone -> ()
                | "ExpiredAt" ->
                    match FsharpMongoHelpers.readTimestamp reader with
                    | ValueSome v -> vExpiredAt <- v
                    | ValueNone -> ()
                | "Version" ->
                    match FsharpMongoHelpers.readInt32 reader with
                    | ValueSome v -> vVersion <- v
                    | ValueNone -> ()
                | _ -> reader.SkipValue()
            | _ -> printfn "Unexpected state: %A" reader.State
        reader.ReadEndDocument()
        {
            Id = vId
            Owner = vOwner
            CurrentConnection = vCurrentConnection
            CurrentMatch = vCurrentMatch
            ExpiredAt = vExpiredAt
            Version = vVersion
        }

    static memberSideFromInt = function
        | Example.GameDomain.Side.Player1 -> Example.GameDomain.Side.Player1
        | Example.GameDomain.Side.Player2 -> Example.GameDomain.Side.Player2
        | _ -> Example.GameDomain.Side.Unknown

    static member GameStatusToBson(writer: IBsonWriter, x: Example.GameDomain.GameStatus) =
        writer.WriteStartDocument()
        match x with
        | Example.GameDomain.GameStatus.InProgress (turn) ->
            writer.WriteName("InProgress")
            writer.WriteInt32(int turn)
        | Example.GameDomain.GameStatus.Finnished (winner,turn) ->
            writer.WriteName("Finnished")
            ConvertExampleGameDomain.GameStatusCaseFinnishedToBson(writer,winner,turn)
        | Example.GameDomain.GameStatus.Terminated ->
            writer.WriteName("Terminated")
            writer.WriteBoolean(true)
        | _ ->
            writer.WriteName("Unknown")
            writer.WriteBoolean(true)
        writer.WriteEndDocument()
    static member GameStatusCaseFinnishedToBson (writer: IBsonWriter,winner,turn) =
        writer.WriteStartDocument()
        writer.WriteName("Winner")
        writer.WriteInt32(winner |> int)
        writer.WriteName("Turn")
        writer.WriteInt32(int turn)
        writer.WriteEndDocument()
    static member GameStatusFromBson (reader: IBsonReader): Example.GameDomain.GameStatus =
        let mutable y = Example.GameDomain.GameStatus.Unknown
        reader.ReadStartDocument()
        match reader.ReadName() with
                | "InProgress" ->
                    let mutable _turn = 0s
                    match FsharpMongoHelpers.readInt16 reader with
                    | ValueSome v -> _turn <- v
                    | ValueNone -> ()
                    y <- _turn |> Example.GameDomain.GameStatus.InProgress
                | "Finnished" ->
                    y <- ConvertExampleGameDomain.GameStatusCaseFinnishedFromBson(reader)
                | "Terminated" ->
                    match FsharpMongoHelpers.readBoolean reader with
                    | ValueSome true -> y <- Example.GameDomain.GameStatus.Terminated
                    | _ -> ()
                | _ -> ()
        reader.ReadEndDocument()
        y
    static member GameStatusCaseFinnishedFromBson (reader: IBsonReader) =
        let mutable winner = Example.GameDomain.Side.Unknown
        let mutable turn = 0s
        reader.ReadStartDocument()
        while reader.State <> BsonReaderState.EndOfDocument do
            match reader.State with
            | BsonReaderState.Type -> reader.ReadBsonType() |> ignore
            | BsonReaderState.Name ->
                match reader.ReadName() with
                | "Winner" ->
                    match FsharpMongoHelpers.readInt reader |> ValueOption.map (fun v -> LanguagePrimitives.EnumOfValue (sbyte v)) with
                    | ValueSome v -> winner <- v
                    | ValueNone -> ()
                | "Turn" ->
                    match FsharpMongoHelpers.readInt16 reader with
                    | ValueSome v -> turn <- v
                    | ValueNone -> ()
                | _ -> reader.SkipValue()
            | _ -> printfn "Unexpected state: %A" reader.State
        reader.ReadEndDocument()
        Example.GameDomain.GameStatus.Finnished (winner,turn)
    static member LocationToBson(writer: IBsonWriter, x: Example.GameDomain.Location) =
        writer.WriteStartDocument()
        writer.WriteName("X")
        writer.WriteInt32(int x.X)
        writer.WriteName("Y")
        writer.WriteInt32(int x.Y)
        writer.WriteEndDocument()

    static member LocationFromBson(reader: IBsonReader): Example.GameDomain.Location =
        let mutable vX = 0y
        let mutable vY = 0y
        reader.ReadStartDocument()
        while reader.State <> BsonReaderState.EndOfDocument do
            match reader.State with
            | BsonReaderState.Type -> reader.ReadBsonType() |> ignore
            | BsonReaderState.Name ->
                match reader.ReadName() with
                | "X" ->
                    match FsharpMongoHelpers.readInt8 reader with
                    | ValueSome v -> vX <- v
                    | ValueNone -> ()
                | "Y" ->
                    match FsharpMongoHelpers.readInt8 reader with
                    | ValueSome v -> vY <- v
                    | ValueNone -> ()
                | _ -> reader.SkipValue()
            | _ -> printfn "Unexpected state: %A" reader.State
        reader.ReadEndDocument()
        {
            X = vX
            Y = vY
        }

    static member UnitToBson(writer: IBsonWriter, x: Example.GameDomain.Unit) =
        writer.WriteStartDocument()
        writer.WriteName("Name")
        writer.WriteString(x.Name)
        writer.WriteName("Health")
        writer.WriteInt32(int x.Health)
        writer.WriteEndDocument()

    static member UnitFromBson(reader: IBsonReader): Example.GameDomain.Unit =
        let mutable vName = ""
        let mutable vHealth = 0y
        reader.ReadStartDocument()
        while reader.State <> BsonReaderState.EndOfDocument do
            match reader.State with
            | BsonReaderState.Type -> reader.ReadBsonType() |> ignore
            | BsonReaderState.Name ->
                match reader.ReadName() with
                | "Name" ->
                    match FsharpMongoHelpers.readString reader with
                    | ValueSome v -> vName <- v
                    | ValueNone -> ()
                | "Health" ->
                    match FsharpMongoHelpers.readInt8 reader with
                    | ValueSome v -> vHealth <- v
                    | ValueNone -> ()
                | _ -> reader.SkipValue()
            | _ -> printfn "Unexpected state: %A" reader.State
        reader.ReadEndDocument()
        {
            Name = vName
            Health = vHealth
        }

    static member GameToBson(writer: IBsonWriter, x: Example.GameDomain.Game, ?asEntity: bool) =
        writer.WriteStartDocument()
        if asEntity.IsSome then
            FsharpMongoHelpers.writeId (writer, x)
        writer.WriteName("Id")
        FsharpMongoHelpers.writeGuid(writer, x.Id)
        writer.WriteName("Player")
        writer.WriteInt32(int x.Player)
        writer.WriteName("Status")
        ConvertExampleGameDomain.GameStatusToBson(writer, x.Status)
        writer.WriteName("Board")
        writer.WriteStartArray()
        for pair in x.Board do
            writer.WriteStartDocument()
            writer.WriteName("Key")
            ConvertExampleGameDomain.LocationToBson(writer, pair.Key)
            writer.WriteName("Value")
            ConvertExampleGameDomain.UnitToBson(writer, pair.Value)
            writer.WriteEndDocument()
        writer.WriteEndArray()
        writer.WriteName("LastChange")
        writer.WriteDateTime(FsharpMongoHelpers.fromDateTime x.LastChange)
        writer.WriteName("Version")
        writer.WriteInt32(x.Version)
        writer.WriteEndDocument()

    static member GameFromBson(reader: IBsonReader): Example.GameDomain.Game =
        let mutable vId = System.Guid.Empty
        let mutable vPlayer = 0y
        let mutable vStatus = Example.GameDomain.GameStatus.Unknown
        let mutable vBoard = ResizeArray()
        let mutable vLastChange = System.DateTime.MinValue
        let mutable vVersion = 0
        reader.ReadStartDocument()
        while reader.State <> BsonReaderState.EndOfDocument do
            match reader.State with
            | BsonReaderState.Type -> reader.ReadBsonType() |> ignore
            | BsonReaderState.Name ->
                match reader.ReadName() with
                | "Id" ->
                    match FsharpMongoHelpers.readGuid reader with
                    | ValueSome v -> vId <- v
                    | ValueNone -> ()
                | "Player" ->
                    match FsharpMongoHelpers.readInt8 reader with
                    | ValueSome v -> vPlayer <- v
                    | ValueNone -> ()
                | "Status" ->
                    match ConvertExampleGameDomain.GameStatusFromBson(reader) |> ValueSome with
                    | ValueSome v -> vStatus <- v
                    | ValueNone -> ()
                | "Board" ->
                    reader.ReadStartArray()
                    while reader.ReadBsonType() <> BsonType.EndOfDocument do
                        reader.ReadStartDocument()
                        let mutable key = Example.GameDomain.Location.Default.Value
                        let mutable value = Example.GameDomain.Unit.Default.Value
                        while reader.ReadBsonType() <> BsonType.EndOfDocument do
                            match reader.ReadName() with
                            | "Key" ->
                                match ConvertExampleGameDomain.LocationFromBson(reader) |> ValueSome with
                                | ValueSome v -> key <- v
                                | ValueNone -> ()
                            | "Value" ->
                                match ConvertExampleGameDomain.UnitFromBson(reader) |> ValueSome with
                                | ValueSome v -> value <- v
                                | ValueNone -> ()
                            | _ -> reader.SkipValue()
                        reader.ReadEndDocument()
                        vBoard.Add(key, value)
                    reader.ReadEndArray()
                | "LastChange" ->
                    match FsharpMongoHelpers.readTimestamp reader with
                    | ValueSome v -> vLastChange <- v
                    | ValueNone -> ()
                | "Version" ->
                    match FsharpMongoHelpers.readInt32 reader with
                    | ValueSome v -> vVersion <- v
                    | ValueNone -> ()
                | _ -> reader.SkipValue()
            | _ -> printfn "Unexpected state: %A" reader.State
        reader.ReadEndDocument()
        {
            Id = vId
            Player = vPlayer
            Status = vStatus
            Board = vBoard |> Map.ofSeq
            LastChange = vLastChange
            Version = vVersion
        }

    static member ActionToBson(writer: IBsonWriter, x: Example.GameDomain.Action) =
        writer.WriteStartDocument()
        match x with
        | Example.GameDomain.Action.EndOfTurn ->
            writer.WriteName("EndOfTurn")
            writer.WriteBoolean(true)
        | Example.GameDomain.Action.Drop (dropPoint) ->
            writer.WriteName("Drop")
            ConvertExampleGameDomain.LocationToBson(writer, dropPoint)
        | Example.GameDomain.Action.Move (fromPoint,toPoint) ->
            writer.WriteName("Move")
            ConvertExampleGameDomain.ActionCaseMoveToBson(writer,fromPoint,toPoint)
        | _ ->
            writer.WriteName("Unknown")
            writer.WriteBoolean(true)
        writer.WriteEndDocument()
    static member ActionCaseMoveToBson (writer: IBsonWriter,fromPoint,toPoint) =
        writer.WriteStartDocument()
        writer.WriteName("FromPoint")
        ConvertExampleGameDomain.LocationToBson(writer, fromPoint)
        writer.WriteName("ToPoint")
        ConvertExampleGameDomain.LocationToBson(writer, toPoint)
        writer.WriteEndDocument()
    static member ActionFromBson (reader: IBsonReader): Example.GameDomain.Action =
        let mutable y = Example.GameDomain.Action.Unknown
        reader.ReadStartDocument()
        match reader.ReadName() with
                | "EndOfTurn" ->
                    match FsharpMongoHelpers.readBoolean reader with
                    | ValueSome true -> y <- Example.GameDomain.Action.EndOfTurn
                    | _ -> ()
                | "Drop" ->
                    let mutable _dropPoint = Example.GameDomain.Location.Default.Value
                    match ConvertExampleGameDomain.LocationFromBson(reader) |> ValueSome with
                    | ValueSome v -> _dropPoint <- v
                    | ValueNone -> ()
                    y <- _dropPoint |> Example.GameDomain.Action.Drop
                | "Move" ->
                    y <- ConvertExampleGameDomain.ActionCaseMoveFromBson(reader)
                | _ -> ()
        reader.ReadEndDocument()
        y
    static member ActionCaseMoveFromBson (reader: IBsonReader) =
        let mutable fromPoint = Example.GameDomain.Location.Default.Value
        let mutable toPoint = Example.GameDomain.Location.Default.Value
        reader.ReadStartDocument()
        while reader.State <> BsonReaderState.EndOfDocument do
            match reader.State with
            | BsonReaderState.Type -> reader.ReadBsonType() |> ignore
            | BsonReaderState.Name ->
                match reader.ReadName() with
                | "FromPoint" ->
                    match ConvertExampleGameDomain.LocationFromBson(reader) |> ValueSome with
                    | ValueSome v -> fromPoint <- v
                    | ValueNone -> ()
                | "ToPoint" ->
                    match ConvertExampleGameDomain.LocationFromBson(reader) |> ValueSome with
                    | ValueSome v -> toPoint <- v
                    | ValueNone -> ()
                | _ -> reader.SkipValue()
            | _ -> printfn "Unexpected state: %A" reader.State
        reader.ReadEndDocument()
        Example.GameDomain.Action.Move (fromPoint,toPoint)
    static member RequestToBson(writer: IBsonWriter, x: Example.GameDomain.Request) =
        writer.WriteStartDocument()
        writer.WriteName("Game")
        ConvertExampleGameDomain.GameToBson(writer, x.Game)
        writer.WriteName("Action")
        ConvertExampleGameDomain.ActionToBson(writer, x.Action)
        writer.WriteEndDocument()

    static member RequestFromBson(reader: IBsonReader): Example.GameDomain.Request =
        let mutable vGame = Example.GameDomain.Game.Default.Value
        let mutable vAction = Example.GameDomain.Action.Unknown
        reader.ReadStartDocument()
        while reader.State <> BsonReaderState.EndOfDocument do
            match reader.State with
            | BsonReaderState.Type -> reader.ReadBsonType() |> ignore
            | BsonReaderState.Name ->
                match reader.ReadName() with
                | "Game" ->
                    match ConvertExampleGameDomain.GameFromBson(reader) |> ValueSome with
                    | ValueSome v -> vGame <- v
                    | ValueNone -> ()
                | "Action" ->
                    match ConvertExampleGameDomain.ActionFromBson(reader) |> ValueSome with
                    | ValueSome v -> vAction <- v
                    | ValueNone -> ()
                | _ -> reader.SkipValue()
            | _ -> printfn "Unexpected state: %A" reader.State
        reader.ReadEndDocument()
        {
            Game = vGame
            Action = vAction
        }

    static member ResponseToBson(writer: IBsonWriter, x: Example.GameDomain.Response) =
        writer.WriteStartDocument()
        match x with
        | Example.GameDomain.Response.Ok (game,possibleActions) ->
            writer.WriteName("Ok")
            ConvertExampleGameDomain.ResponseCaseOkToBson(writer,game,possibleActions)
        | Example.GameDomain.Response.Fail (errors) ->
            writer.WriteName("Fail")
            writer.WriteStartArray()
            for v in errors do
                writer.WriteString(v)
            writer.WriteEndArray()
        | _ ->
            writer.WriteName("Unknown")
            writer.WriteBoolean(true)
        writer.WriteEndDocument()
    static member ResponseCaseOkToBson (writer: IBsonWriter,game,possibleActions) =
        writer.WriteStartDocument()
        writer.WriteName("Game")
        ConvertExampleGameDomain.GameToBson(writer, game)
        writer.WriteName("PossibleActions")
        writer.WriteStartArray()
        for v in possibleActions do
            ConvertExampleGameDomain.ActionToBson(writer, v)
        writer.WriteEndArray()
        writer.WriteEndDocument()
    static member ResponseFromBson (reader: IBsonReader): Example.GameDomain.Response =
        let mutable y = Example.GameDomain.Response.Unknown
        reader.ReadStartDocument()
        match reader.ReadName() with
                | "Ok" ->
                    y <- ConvertExampleGameDomain.ResponseCaseOkFromBson(reader)
                | "Fail" ->
                    let mutable _errors = ResizeArray()
                    reader.ReadStartArray()
                    while reader.ReadBsonType() <> BsonType.EndOfDocument do
                        match FsharpMongoHelpers.readString reader with
                        | ValueSome v -> _errors.Add(v)
                        | ValueNone -> ()
                    reader.ReadEndArray()
                    y <- _errors |> List.ofSeq |> Example.GameDomain.Response.Fail
                | _ -> ()
        reader.ReadEndDocument()
        y
    static member ResponseCaseOkFromBson (reader: IBsonReader) =
        let mutable game = Example.GameDomain.Game.Default.Value
        let mutable possibleActions = ResizeArray()
        reader.ReadStartDocument()
        while reader.State <> BsonReaderState.EndOfDocument do
            match reader.State with
            | BsonReaderState.Type -> reader.ReadBsonType() |> ignore
            | BsonReaderState.Name ->
                match reader.ReadName() with
                | "Game" ->
                    match ConvertExampleGameDomain.GameFromBson(reader) |> ValueSome with
                    | ValueSome v -> game <- v
                    | ValueNone -> ()
                | "PossibleActions" ->
                    reader.ReadStartArray()
                    while reader.ReadBsonType() <> BsonType.EndOfDocument do
                        match ConvertExampleGameDomain.ActionFromBson(reader) |> ValueSome with
                        | ValueSome v -> possibleActions.Add(v)
                        | ValueNone -> ()
                    reader.ReadEndArray()
                | _ -> reader.SkipValue()
            | _ -> printfn "Unexpected state: %A" reader.State
        reader.ReadEndDocument()
        Example.GameDomain.Response.Ok (game,possibleActions |> List.ofSeq)

type SessionSerializer() =
    inherit SerializerBase<Example.GameDomain.Session>()
    override x.Deserialize(ctx: BsonDeserializationContext, args: BsonDeserializationArgs) =
        ConvertExampleGameDomain.SessionFromBson(ctx.Reader)

    override x.Serialize(ctx: BsonSerializationContext, args: BsonSerializationArgs, value: Example.GameDomain.Session) =
        ConvertExampleGameDomain.SessionToBson(ctx.Writer, value, true)

type GameSerializer() =
    inherit SerializerBase<Example.GameDomain.Game>()
    override x.Deserialize(ctx: BsonDeserializationContext, args: BsonDeserializationArgs) =
        ConvertExampleGameDomain.GameFromBson(ctx.Reader)

    override x.Serialize(ctx: BsonSerializationContext, args: BsonSerializationArgs, value: Example.GameDomain.Game) =
        ConvertExampleGameDomain.GameToBson(ctx.Writer, value, true)

type ConvertExampleGameDomain with
    static member RegisterSerializers() =
        BsonDefaults.GuidRepresentationMode <- GuidRepresentationMode.V3
        BsonSerializer.RegisterSerializer(SessionSerializer())
        BsonSerializer.RegisterSerializer(GameSerializer())
