namespace Protokeep.FsharpJson

open System.Text.Json
open Protokeep

type ConvertExampleGameDomain() =

    static member SessionOwnerFromJson(reader: byref<Utf8JsonReader>): Example.GameDomain.SessionOwner voption =
        if FsharpJsonHelpers.moveToStartObject(&reader)then
            let mutable y = Example.GameDomain.SessionOwner.Unknown
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Guest")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.True
                    then y <- Example.GameDomain.SessionOwner.Guest
                    else reader.Skip()
                else if (reader.ValueTextEquals("Registered")) then
                    let mutable _playerId = System.Guid.Empty
                    match FsharpJsonHelpers.readGuid(&reader) with
                    | ValueSome v -> _playerId <- v
                    | ValueNone -> ()
                    y <- _playerId |> Example.GameDomain.SessionOwner.Registered
                else reader.Skip()
            ValueSome y
        else ValueNone
    static member SessionOwnerToJson (writer:inref<Utf8JsonWriter>, x: Example.GameDomain.SessionOwner) =
        writer.WriteStartObject()
        match x with
        | Example.GameDomain.SessionOwner.Guest ->
            writer.WritePropertyName("Guest")
            writer.WriteBooleanValue(true)
        | Example.GameDomain.SessionOwner.Registered (playerId) ->
            writer.WritePropertyName("Registered")
            FsharpJsonHelpers.writeGuid(&writer, playerId)
        | _ ->
            writer.WritePropertyName("Unknown")
            writer.WriteBooleanValue(true)
        writer.WriteEndObject()

    static member ConnectionToJson (writer: inref<Utf8JsonWriter>, x: Example.GameDomain.Connection) =
        writer.WriteStartObject()
        writer.WritePropertyName("Id")
        writer.WriteStringValue(x.Id)
        writer.WriteEndObject()

    static member ConnectionFromJson(reader: byref<Utf8JsonReader>): Example.GameDomain.Connection voption =
        let mutable vId = ""
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Id")) then
                    match FsharpJsonHelpers.readString(&reader) with
                    | ValueSome v -> vId <- v
                    | ValueNone -> ()
                else reader.Skip()
            ValueSome {
                Id = vId
            }
        else ValueNone
    static member SessionToJson (writer: inref<Utf8JsonWriter>, x: Example.GameDomain.Session) =
        writer.WriteStartObject()
        writer.WritePropertyName("Id")
        FsharpJsonHelpers.writeGuid(&writer, x.Id)
        writer.WritePropertyName("Owner")
        ConvertExampleGameDomain.SessionOwnerToJson(&writer, x.Owner)
        match x.CurrentConnection with
        | ValueSome v ->
            writer.WritePropertyName("CurrentConnectionValue")
            ConvertExampleGameDomain.ConnectionToJson(&writer, v)
        | ValueNone -> ()
        match x.CurrentMatch with
        | ValueSome v ->
            writer.WritePropertyName("CurrentMatchValue")
            FsharpJsonHelpers.writeGuid(&writer, v)
        | ValueNone -> ()
        writer.WritePropertyName("ExpiredAt")
        FsharpJsonHelpers.writeTimestamp(&writer, x.ExpiredAt)
        writer.WritePropertyName("Version")
        writer.WriteNumberValue(x.Version)
        writer.WriteEndObject()

    static member SessionFromJson(reader: byref<Utf8JsonReader>): Example.GameDomain.Session voption =
        let mutable vId = System.Guid.Empty
        let mutable vOwner = Example.GameDomain.SessionOwner.Unknown
        let mutable vCurrentConnection = ValueNone
        let mutable vCurrentMatch = ValueNone
        let mutable vExpiredAt = System.DateTime.MinValue
        let mutable vVersion = 0
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Id")) then
                    match FsharpJsonHelpers.readGuid(&reader) with
                    | ValueSome v -> vId <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Owner")) then
                    match ConvertExampleGameDomain.SessionOwnerFromJson(&reader) with
                    | ValueSome v -> vOwner <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("CurrentConnectionValue")) then
                    match ConvertExampleGameDomain.ConnectionFromJson(&reader) |> ValueOption.map ValueSome with
                    | ValueSome v -> vCurrentConnection <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("CurrentMatchValue")) then
                    match FsharpJsonHelpers.readGuid(&reader) |> ValueOption.map ValueSome with
                    | ValueSome v -> vCurrentMatch <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("ExpiredAt")) then
                    match FsharpJsonHelpers.readTimestamp(&reader) with
                    | ValueSome v -> vExpiredAt <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Version")) then
                    match FsharpJsonHelpers.readInt(&reader) with
                    | ValueSome v -> vVersion <- v
                    | ValueNone -> ()
                else reader.Skip()
            ValueSome {
                Id = vId
                Owner = vOwner
                CurrentConnection = vCurrentConnection
                CurrentMatch = vCurrentMatch
                ExpiredAt = vExpiredAt
                Version = vVersion
            }
        else ValueNone
    static member SideFromString =
        function
        | "SidePlayer1" -> Example.GameDomain.Side.Player1
        | "SidePlayer2" -> Example.GameDomain.Side.Player2
        | _ -> Example.GameDomain.Side.Unknown

    static member SideToString =
        function
        | Example.GameDomain.Side.Player1 -> "SidePlayer1"
        | Example.GameDomain.Side.Player2 -> "SidePlayer2"
        | _ -> "Unknown"

    static member GameStatusFromJson(reader: byref<Utf8JsonReader>): Example.GameDomain.GameStatus voption =
        if FsharpJsonHelpers.moveToStartObject(&reader)then
            let mutable y = Example.GameDomain.GameStatus.Unknown
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("InProgress")) then
                    let mutable _turn = 0s
                    match FsharpJsonHelpers.readShort(&reader) with
                    | ValueSome v -> _turn <- v
                    | ValueNone -> ()
                    y <- _turn |> Example.GameDomain.GameStatus.InProgress
                else if (reader.ValueTextEquals("Finnished")) then
                    y <- ConvertExampleGameDomain.GameStatusCaseFinnishedFromJson(&reader)
                else if (reader.ValueTextEquals("Terminated")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.True
                    then y <- Example.GameDomain.GameStatus.Terminated
                    else reader.Skip()
                else reader.Skip()
            ValueSome y
        else ValueNone
    static member GameStatusToJson (writer:inref<Utf8JsonWriter>, x: Example.GameDomain.GameStatus) =
        writer.WriteStartObject()
        match x with
        | Example.GameDomain.GameStatus.InProgress (turn) ->
            writer.WritePropertyName("InProgress")
            writer.WriteNumberValue(turn)
        | Example.GameDomain.GameStatus.Finnished (winner,turn) ->
            writer.WritePropertyName("Finnished")
            ConvertExampleGameDomain.GameStatusCaseFinnishedToJson(&writer,winner,turn)
        | Example.GameDomain.GameStatus.Terminated ->
            writer.WritePropertyName("Terminated")
            writer.WriteBooleanValue(true)
        | _ ->
            writer.WritePropertyName("Unknown")
            writer.WriteBooleanValue(true)
        writer.WriteEndObject()
    static member GameStatusCaseFinnishedFromJson(reader: byref<Utf8JsonReader>) =
        let mutable winner = Example.GameDomain.Side.Unknown
        let mutable turn = 0s
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Winner")) then
                    match FsharpJsonHelpers.readString(&reader) |> ValueOption.map ConvertExampleGameDomain.SideFromString with
                    | ValueSome v -> winner <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Turn")) then
                    match FsharpJsonHelpers.readShort(&reader) with
                    | ValueSome v -> turn <- v
                    | ValueNone -> ()
                else reader.Skip()
        Example.GameDomain.GameStatus.Finnished (winner,turn)
    static member GameStatusCaseFinnishedToJson (writer: inref<Utf8JsonWriter>,winner,turn) =
        writer.WriteStartObject()
        writer.WritePropertyName("Winner")
        writer.WriteStringValue(winner |> ConvertExampleGameDomain.SideToString)
        writer.WritePropertyName("Turn")
        writer.WriteNumberValue(turn)
        writer.WriteEndObject()

    static member LocationToJson (writer: inref<Utf8JsonWriter>, x: Example.GameDomain.Location) =
        writer.WriteStartObject()
        writer.WritePropertyName("X")
        writer.WriteNumberValue(x.X)
        writer.WritePropertyName("Y")
        writer.WriteNumberValue(x.Y)
        writer.WriteEndObject()

    static member LocationFromJson(reader: byref<Utf8JsonReader>): Example.GameDomain.Location voption =
        let mutable vX = 0y
        let mutable vY = 0y
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("X")) then
                    match FsharpJsonHelpers.readByte(&reader) with
                    | ValueSome v -> vX <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Y")) then
                    match FsharpJsonHelpers.readByte(&reader) with
                    | ValueSome v -> vY <- v
                    | ValueNone -> ()
                else reader.Skip()
            ValueSome {
                X = vX
                Y = vY
            }
        else ValueNone
    static member UnitToJson (writer: inref<Utf8JsonWriter>, x: Example.GameDomain.Unit) =
        writer.WriteStartObject()
        writer.WritePropertyName("Name")
        writer.WriteStringValue(x.Name)
        writer.WritePropertyName("Health")
        writer.WriteNumberValue(x.Health)
        writer.WriteEndObject()

    static member UnitFromJson(reader: byref<Utf8JsonReader>): Example.GameDomain.Unit voption =
        let mutable vName = ""
        let mutable vHealth = 0y
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Name")) then
                    match FsharpJsonHelpers.readString(&reader) with
                    | ValueSome v -> vName <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Health")) then
                    match FsharpJsonHelpers.readByte(&reader) with
                    | ValueSome v -> vHealth <- v
                    | ValueNone -> ()
                else reader.Skip()
            ValueSome {
                Name = vName
                Health = vHealth
            }
        else ValueNone
    static member GameToJson (writer: inref<Utf8JsonWriter>, x: Example.GameDomain.Game) =
        writer.WriteStartObject()
        writer.WritePropertyName("Id")
        FsharpJsonHelpers.writeGuid(&writer, x.Id)
        writer.WritePropertyName("Player")
        writer.WriteNumberValue(x.Player)
        writer.WritePropertyName("Status")
        ConvertExampleGameDomain.GameStatusToJson(&writer, x.Status)
        writer.WritePropertyName("Board")
        writer.WriteStartArray()
        for pair in x.Board do
            writer.WriteStartObject()
            writer.WritePropertyName("Key")
            ConvertExampleGameDomain.LocationToJson(&writer, pair.Key)
            writer.WritePropertyName("Value")
            ConvertExampleGameDomain.UnitToJson(&writer, pair.Value)
            writer.WriteEndObject()
        writer.WriteEndArray()
        writer.WritePropertyName("LastChange")
        FsharpJsonHelpers.writeTimestamp(&writer, x.LastChange)
        writer.WritePropertyName("Version")
        writer.WriteNumberValue(x.Version)
        writer.WriteEndObject()

    static member GameFromJson(reader: byref<Utf8JsonReader>): Example.GameDomain.Game voption =
        let mutable vId = System.Guid.Empty
        let mutable vPlayer = 0y
        let mutable vStatus = Example.GameDomain.GameStatus.Unknown
        let mutable vBoard = ResizeArray()
        let mutable vLastChange = System.DateTime.MinValue
        let mutable vVersion = 0
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Id")) then
                    match FsharpJsonHelpers.readGuid(&reader) with
                    | ValueSome v -> vId <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Player")) then
                    match FsharpJsonHelpers.readByte(&reader) with
                    | ValueSome v -> vPlayer <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Status")) then
                    match ConvertExampleGameDomain.GameStatusFromJson(&reader) with
                    | ValueSome v -> vStatus <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Board")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.StartArray then
                        while reader.Read() && reader.TokenType <> JsonTokenType.EndArray do
                            if reader.TokenType = JsonTokenType.StartObject then
                                let mutable key = Example.GameDomain.Location.Default.Value
                                let mutable value = Example.GameDomain.Unit.Default.Value
                                while reader.Read() && reader.TokenType <> JsonTokenType.EndObject do
                                    if (reader.ValueTextEquals("Key")) then
                                        match ConvertExampleGameDomain.LocationFromJson(&reader) with
                                        | ValueSome v -> key <- v
                                        | ValueNone -> ()
                                    else if (reader.ValueTextEquals("Value")) then
                                        match ConvertExampleGameDomain.UnitFromJson(&reader) with
                                        | ValueSome v -> value <- v
                                        | ValueNone -> ()
                                    else reader.Skip()
                                vBoard.Add(key, value)
                            else reader.Skip()
                    else reader.Skip()
                else if (reader.ValueTextEquals("LastChange")) then
                    match FsharpJsonHelpers.readTimestamp(&reader) with
                    | ValueSome v -> vLastChange <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Version")) then
                    match FsharpJsonHelpers.readInt(&reader) with
                    | ValueSome v -> vVersion <- v
                    | ValueNone -> ()
                else reader.Skip()
            ValueSome {
                Id = vId
                Player = vPlayer
                Status = vStatus
                Board = vBoard |> Map.ofSeq
                LastChange = vLastChange
                Version = vVersion
            }
        else ValueNone
    static member ActionFromJson(reader: byref<Utf8JsonReader>): Example.GameDomain.Action voption =
        if FsharpJsonHelpers.moveToStartObject(&reader)then
            let mutable y = Example.GameDomain.Action.Unknown
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("EndOfTurn")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.True
                    then y <- Example.GameDomain.Action.EndOfTurn
                    else reader.Skip()
                else if (reader.ValueTextEquals("Drop")) then
                    let mutable _dropPoint = Example.GameDomain.Location.Default.Value
                    match ConvertExampleGameDomain.LocationFromJson(&reader) with
                    | ValueSome v -> _dropPoint <- v
                    | ValueNone -> ()
                    y <- _dropPoint |> Example.GameDomain.Action.Drop
                else if (reader.ValueTextEquals("Move")) then
                    y <- ConvertExampleGameDomain.ActionCaseMoveFromJson(&reader)
                else reader.Skip()
            ValueSome y
        else ValueNone
    static member ActionToJson (writer:inref<Utf8JsonWriter>, x: Example.GameDomain.Action) =
        writer.WriteStartObject()
        match x with
        | Example.GameDomain.Action.EndOfTurn ->
            writer.WritePropertyName("EndOfTurn")
            writer.WriteBooleanValue(true)
        | Example.GameDomain.Action.Drop (dropPoint) ->
            writer.WritePropertyName("Drop")
            ConvertExampleGameDomain.LocationToJson(&writer, dropPoint)
        | Example.GameDomain.Action.Move (fromPoint,toPoint) ->
            writer.WritePropertyName("Move")
            ConvertExampleGameDomain.ActionCaseMoveToJson(&writer,fromPoint,toPoint)
        | _ ->
            writer.WritePropertyName("Unknown")
            writer.WriteBooleanValue(true)
        writer.WriteEndObject()
    static member ActionCaseMoveFromJson(reader: byref<Utf8JsonReader>) =
        let mutable fromPoint = Example.GameDomain.Location.Default.Value
        let mutable toPoint = Example.GameDomain.Location.Default.Value
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("FromPoint")) then
                    match ConvertExampleGameDomain.LocationFromJson(&reader) with
                    | ValueSome v -> fromPoint <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("ToPoint")) then
                    match ConvertExampleGameDomain.LocationFromJson(&reader) with
                    | ValueSome v -> toPoint <- v
                    | ValueNone -> ()
                else reader.Skip()
        Example.GameDomain.Action.Move (fromPoint,toPoint)
    static member ActionCaseMoveToJson (writer: inref<Utf8JsonWriter>,fromPoint,toPoint) =
        writer.WriteStartObject()
        writer.WritePropertyName("FromPoint")
        ConvertExampleGameDomain.LocationToJson(&writer, fromPoint)
        writer.WritePropertyName("ToPoint")
        ConvertExampleGameDomain.LocationToJson(&writer, toPoint)
        writer.WriteEndObject()

    static member RequestToJson (writer: inref<Utf8JsonWriter>, x: Example.GameDomain.Request) =
        writer.WriteStartObject()
        writer.WritePropertyName("Game")
        ConvertExampleGameDomain.GameToJson(&writer, x.Game)
        writer.WritePropertyName("Action")
        ConvertExampleGameDomain.ActionToJson(&writer, x.Action)
        writer.WriteEndObject()

    static member RequestFromJson(reader: byref<Utf8JsonReader>): Example.GameDomain.Request voption =
        let mutable vGame = Example.GameDomain.Game.Default.Value
        let mutable vAction = Example.GameDomain.Action.Unknown
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Game")) then
                    match ConvertExampleGameDomain.GameFromJson(&reader) with
                    | ValueSome v -> vGame <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Action")) then
                    match ConvertExampleGameDomain.ActionFromJson(&reader) with
                    | ValueSome v -> vAction <- v
                    | ValueNone -> ()
                else reader.Skip()
            ValueSome {
                Game = vGame
                Action = vAction
            }
        else ValueNone
    static member ResponseFromJson(reader: byref<Utf8JsonReader>): Example.GameDomain.Response voption =
        if FsharpJsonHelpers.moveToStartObject(&reader)then
            let mutable y = Example.GameDomain.Response.Unknown
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Ok")) then
                    y <- ConvertExampleGameDomain.ResponseCaseOkFromJson(&reader)
                else if (reader.ValueTextEquals("Fail")) then
                    let mutable _errors = ResizeArray()
                    if reader.Read() && reader.TokenType = JsonTokenType.StartArray then
                        while reader.TokenType <> JsonTokenType.EndArray do
                            match FsharpJsonHelpers.readString(&reader) with
                            | ValueSome v -> _errors.Add(v)
                            | ValueNone -> ()
                    else reader.Skip()
                    y <- _errors |> List.ofSeq |> Example.GameDomain.Response.Fail
                else reader.Skip()
            ValueSome y
        else ValueNone
    static member ResponseToJson (writer:inref<Utf8JsonWriter>, x: Example.GameDomain.Response) =
        writer.WriteStartObject()
        match x with
        | Example.GameDomain.Response.Ok (game,possibleActions) ->
            writer.WritePropertyName("Ok")
            ConvertExampleGameDomain.ResponseCaseOkToJson(&writer,game,possibleActions)
        | Example.GameDomain.Response.Fail (errors) ->
            writer.WritePropertyName("Fail")
            writer.WriteStartArray()
            for v in errors do
                writer.WriteStringValue(v)
            writer.WriteEndArray()
        | _ ->
            writer.WritePropertyName("Unknown")
            writer.WriteBooleanValue(true)
        writer.WriteEndObject()
    static member ResponseCaseOkFromJson(reader: byref<Utf8JsonReader>) =
        let mutable game = Example.GameDomain.Game.Default.Value
        let mutable possibleActions = ResizeArray()
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Game")) then
                    match ConvertExampleGameDomain.GameFromJson(&reader) with
                    | ValueSome v -> game <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("PossibleActions")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.StartArray then
                        while reader.TokenType <> JsonTokenType.EndArray do
                            match ConvertExampleGameDomain.ActionFromJson(&reader) with
                            | ValueSome v -> possibleActions.Add(v)
                            | ValueNone -> ()
                    else reader.Skip()
                else reader.Skip()
        Example.GameDomain.Response.Ok (game,possibleActions |> List.ofSeq)
    static member ResponseCaseOkToJson (writer: inref<Utf8JsonWriter>,game,possibleActions) =
        writer.WriteStartObject()
        writer.WritePropertyName("Game")
        ConvertExampleGameDomain.GameToJson(&writer, game)
        writer.WritePropertyName("PossibleActions")
        writer.WriteStartArray()
        for v in possibleActions do
            ConvertExampleGameDomain.ActionToJson(&writer, v)
        writer.WriteEndArray()
        writer.WriteEndObject()

