namespace Protokeep

module FsharpTypes =
    type Key =
        | Value of string
        | Items of Key list
        | Inner of Key

        override x.ToString() =
            match x with
            | Value v -> v
            | Items keys -> keys |> List.map (fun key -> key.ToString()) |> String.concat ","
            | Inner key -> $"({key})"

    let (|TryFind|_|) f key = f key

    type IEntity =
        abstract member Key: Key

    type IVersioned =
        abstract member Version: int with get, set


module FsharpMongoHelpers =
    open System
    open MongoDB.Bson
    open FsharpTypes

    let writeId (writer: IO.IBsonWriter, entity: IEntity) =
        writer.WriteName("_id")
        writer.WriteString(entity.Key.ToString())

    let private unixEpoch = DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)

    let toDateTime (milliseconds: int64) : DateTime =
        unixEpoch.AddMilliseconds(float milliseconds)

    let fromDateTime (dateTime: DateTime) : int64 =
        int64 (dateTime - unixEpoch).TotalMilliseconds

    let readTimestamp (reader: IO.IBsonReader) =
        match reader.CurrentBsonType with
        | BsonType.DateTime -> reader.ReadDateTime() |> toDateTime |> ValueSome
        | bsonType -> ValueNone

    let toMoney (value: decimal, scale: int) : int =
        int (value * decimal (Math.Pow(10., float scale)))

    let fromMoney (value: int, scale: int) : decimal =
        decimal value / decimal (Math.Pow(10., float scale))

    let readInt (reader: IO.IBsonReader) =
        match reader.CurrentBsonType with
        | BsonType.Int32 -> reader.ReadInt32() |> ValueSome
        | BsonType.Int64 -> int (reader.ReadInt64()) |> ValueSome
        | BsonType.Double -> int (reader.ReadDouble()) |> ValueSome
        | BsonType.String -> int (reader.ReadString()) |> ValueSome
        | _ ->
            reader.SkipValue()
            ValueNone

    let readLong (reader: IO.IBsonReader) =
        match reader.CurrentBsonType with
        | BsonType.Int32 -> int64 (reader.ReadInt32()) |> ValueSome
        | BsonType.Int64 -> reader.ReadInt64() |> ValueSome
        | BsonType.Double -> int64 (reader.ReadDouble()) |> ValueSome
        | BsonType.String -> int64 (reader.ReadString()) |> ValueSome
        | _ ->
            reader.SkipValue()
            ValueNone

    let readFloat (reader: IO.IBsonReader) =
        match reader.CurrentBsonType with
        | BsonType.Int32 -> float32 (reader.ReadInt32()) |> ValueSome
        | BsonType.Int64 -> float32 (reader.ReadInt64()) |> ValueSome
        | BsonType.Double -> reader.ReadDouble() |> float32 |> ValueSome
        | BsonType.String -> reader.ReadString() |> float32 |> ValueSome
        | _ ->
            reader.SkipValue()
            ValueNone

    let readDouble (reader: IO.IBsonReader) =
        match reader.CurrentBsonType with
        | BsonType.Int32 -> float (reader.ReadInt32()) |> ValueSome
        | BsonType.Int64 -> float (reader.ReadInt64()) |> ValueSome
        | BsonType.Double -> reader.ReadDouble() |> ValueSome
        | BsonType.String -> reader.ReadString() |> float |> ValueSome
        | _ ->
            reader.SkipValue()
            ValueNone

    let readBoolean (reader: IO.IBsonReader) =
        match reader.CurrentBsonType with
        | BsonType.Boolean -> reader.ReadBoolean() |> ValueSome
        | BsonType.Int32 -> reader.ReadInt32() |> (fun v -> v <> 0) |> ValueSome
        | _ ->
            reader.SkipValue()
            ValueNone

    let readString (reader: IO.IBsonReader) =
        match reader.CurrentBsonType with
        | BsonType.String -> reader.ReadString() |> ValueSome
        | BsonType.Int32 -> reader.ReadInt32() |> string |> ValueSome
        | BsonType.Int64 -> reader.ReadInt64() |> string |> ValueSome
        | BsonType.Double -> reader.ReadDouble() |> string |> ValueSome
        | _ ->
            reader.SkipValue()
            ValueNone

    let readBytes (reader: IO.IBsonReader) =
        match reader.CurrentBsonType with
        | BsonType.Binary -> reader.ReadBinaryData().Bytes |> ValueSome
        | _ ->
            reader.SkipValue()
            ValueNone

    let readMoney (reader: IO.IBsonReader, scale: int) =
        readInt reader |> ValueOption.map (fun v -> fromMoney (v, scale))

    let readDuration (reader: IO.IBsonReader) =
        readInt reader |> ValueOption.map TimeSpan.FromMilliseconds

    let readGuid (reader: IO.IBsonReader) =
        match reader.CurrentBsonType with
        | BsonType.Binary -> reader.ReadBinaryData().ToGuid() |> ValueSome
        | _ ->
            reader.SkipValue()
            ValueNone

    let writeGuid (writer: IO.IBsonWriter, value: Guid) =
        writer.WriteBinaryData(
            BsonBinaryData(
                GuidConverter.ToBytes(value, GuidRepresentation.Standard),
                GuidConverter.GetSubType(GuidRepresentation.Standard)
            )
        )

module FsharpJsonHelpers =
    open System.Text.Json

    let fromDateTime (v: System.DateTime) = v.ToString("O")

    let durationRegex =
        System.Text.RegularExpressions.Regex @"^(-)?([0-9]{1,12})(\.[0-9]{1,9})?s$"

    let subsecondScalingFactors =
        [| 0
           100000000
           100000000
           10000000
           1000000
           100000
           10000
           1000
           100
           10
           1 |]

    let toTimeSpan (v: string) =
        let m = durationRegex.Match(v)

        match m.Success with
        | true ->
            let signText = m.Groups.[1].Value
            let secondsText = m.Groups.[2].Value
            let subseconds = m.Groups.[3].Value
            let sign = if signText = "-" then -1. else 1.

            let seconds = System.Int64.Parse(secondsText) |> float

            let milliseconds =
                if subseconds <> "" then
                    let parsedFraction = System.Int32.Parse(subseconds.Substring(1))

                    parsedFraction * (subsecondScalingFactors.[subseconds.Length]) / 1000000
                    |> float
                else
                    0.

            System.TimeSpan.FromMilliseconds(sign * (seconds * 1000. + milliseconds))
        | false -> failwithf "Invalid Duration value: %s" v

    let fromTimeSpan (v: System.TimeSpan) =
        sprintf "%d.%ds" (int64 v.TotalSeconds) v.Milliseconds

    let moveToStartObject (reader: byref<Utf8JsonReader>) =
        reader.TokenType = JsonTokenType.StartObject
        || reader.Read() && reader.TokenType = JsonTokenType.StartObject

    let moveToEndObject (reader: byref<Utf8JsonReader>) =
        reader.Read() = false || reader.TokenType = JsonTokenType.EndObject
