module FsharpMongoHelpers =
    open System
    open MongoDB.Bson
    open Protokeep.FsharpTypes

    let writeId (writer: IO.IBsonWriter, key: Key) =
        writer.WriteName("_id")
        writer.WriteString(key.ToString())

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
