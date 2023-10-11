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

    let ReadBsonType (reader: IO.IBsonReader) =
        match reader.CurrentBsonType with
        | BsonType.Int32 -> sbyte (reader.ReadInt32()) |> ValueSome
        | BsonType.Int64 -> sbyte (reader.ReadInt64()) |> ValueSome
        | BsonType.Double -> sbyte (reader.ReadDouble()) |> ValueSome
        | BsonType.String -> sbyte (reader.ReadString()) |> ValueSome
        | _ ->
            reader.SkipValue()
            ValueNone

    let readInt8 (reader: IO.IBsonReader) =
        match reader.CurrentBsonType with
        | BsonType.Int32 -> sbyte (reader.ReadInt32()) |> ValueSome
        | BsonType.Int64 -> sbyte (reader.ReadInt64()) |> ValueSome
        | BsonType.Double -> sbyte (reader.ReadDouble()) |> ValueSome
        | BsonType.String -> sbyte (reader.ReadString()) |> ValueSome
        | _ ->
            reader.SkipValue()
            ValueNone

    let readInt16 (reader: IO.IBsonReader) =
        match reader.CurrentBsonType with
        | BsonType.Int32 -> int16 (reader.ReadInt32()) |> ValueSome
        | BsonType.Int64 -> int16 (reader.ReadInt64()) |> ValueSome
        | BsonType.Double -> int16 (reader.ReadDouble()) |> ValueSome
        | BsonType.String -> int16 (reader.ReadString()) |> ValueSome
        | _ ->
            reader.SkipValue()
            ValueNone

    let readInt32 (reader: IO.IBsonReader) =
        match reader.CurrentBsonType with
        | BsonType.Int32 -> reader.ReadInt32() |> ValueSome
        | BsonType.Int64 -> int (reader.ReadInt64()) |> ValueSome
        | BsonType.Double -> int (reader.ReadDouble()) |> ValueSome
        | BsonType.String -> int (reader.ReadString()) |> ValueSome
        | _ ->
            reader.SkipValue()
            ValueNone

    let readInt64 (reader: IO.IBsonReader) =
        match reader.CurrentBsonType with
        | BsonType.Int32 -> int64 (reader.ReadInt32()) |> ValueSome
        | BsonType.Int64 -> reader.ReadInt64() |> ValueSome
        | BsonType.Double -> int64 (reader.ReadDouble()) |> ValueSome
        | BsonType.String -> int64 (reader.ReadString()) |> ValueSome
        | _ ->
            reader.SkipValue()
            ValueNone

    let readFloat32 (reader: IO.IBsonReader) =
        match reader.CurrentBsonType with
        | BsonType.Int32 -> float32 (reader.ReadInt32()) |> ValueSome
        | BsonType.Int64 -> float32 (reader.ReadInt64()) |> ValueSome
        | BsonType.Double -> reader.ReadDouble() |> float32 |> ValueSome
        | BsonType.String -> reader.ReadString() |> float32 |> ValueSome
        | _ ->
            reader.SkipValue()
            ValueNone

    let readFloat64 (reader: IO.IBsonReader) =
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

    let readBinary (reader: IO.IBsonReader) =
        match reader.CurrentBsonType with
        | BsonType.Binary -> reader.ReadBinaryData().Bytes |> ValueSome
        | _ ->
            reader.SkipValue()
            ValueNone

    let readMoney (reader: IO.IBsonReader, scale: int) =
        readInt32 reader |> ValueOption.map (fun v -> fromMoney (v, scale))

    let readDuration (reader: IO.IBsonReader) =
        readInt32 reader |> ValueOption.map TimeSpan.FromMilliseconds

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
