namespace Protokeep

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

    let readString (reader: byref<Utf8JsonReader>) =
        match reader.Read() with
        | true ->
            match reader.TokenType with
            | JsonTokenType.String -> reader.GetString() |> ValueSome
            | JsonTokenType.Number -> reader.GetDecimal().ToString() |> ValueSome
            | _ ->
                reader.Skip()
                ValueNone
        | false -> ValueNone

    let readByte (reader: byref<Utf8JsonReader>) =
        match reader.Read() with
        | true ->
            match reader.TokenType with
            | JsonTokenType.Number ->
                try
                    reader.GetSByte()
                with _ ->
                    reader.GetDecimal() |> sbyte
                |> ValueSome
            | JsonTokenType.String -> reader.GetString() |> sbyte |> ValueSome
            | _ ->
                reader.Skip()
                ValueNone
        | false -> ValueNone

    let readShort (reader: byref<Utf8JsonReader>) =
        match reader.Read() with
        | true ->
            match reader.TokenType with
            | JsonTokenType.Number ->
                try
                    reader.GetInt16()
                with _ ->
                    reader.GetDecimal() |> int16
                |> ValueSome
            | JsonTokenType.String -> reader.GetString() |> int16 |> ValueSome
            | _ ->
                reader.Skip()
                ValueNone
        | false -> ValueNone

    let readInt (reader: byref<Utf8JsonReader>) =
        match reader.Read() with
        | true ->
            match reader.TokenType with
            | JsonTokenType.Number ->
                try
                    reader.GetInt32()
                with _ ->
                    reader.GetDecimal() |> int
                |> ValueSome
            | JsonTokenType.String -> reader.GetString() |> int |> ValueSome
            | _ ->
                reader.Skip()
                ValueNone
        | false -> ValueNone

    let readLong (reader: byref<Utf8JsonReader>) =
        match reader.Read() with
        | true ->
            match reader.TokenType with
            | JsonTokenType.Number ->
                try
                    reader.GetInt64()
                with _ ->
                    reader.GetDecimal() |> int64
                |> ValueSome
            | JsonTokenType.String -> reader.GetString() |> int64 |> ValueSome
            | _ ->
                reader.Skip()
                ValueNone
        | false -> ValueNone

    let readSingle (reader: byref<Utf8JsonReader>) =
        match reader.Read() with
        | true ->
            match reader.TokenType with
            | JsonTokenType.Number -> reader.GetSingle() |> ValueSome
            | JsonTokenType.String -> reader.GetString() |> float32 |> ValueSome
            | _ ->
                reader.Skip()
                ValueNone
        | false -> ValueNone

    let readDouble (reader: byref<Utf8JsonReader>) =
        match reader.Read() with
        | true ->
            match reader.TokenType with
            | JsonTokenType.Number -> reader.GetDouble() |> ValueSome
            | JsonTokenType.String -> reader.GetString() |> double |> ValueSome
            | _ ->
                reader.Skip()
                ValueNone
        | false -> ValueNone

    let readMoney (reader: byref<Utf8JsonReader>, scale: int) =
        match reader.Read() with
        | true ->
            match reader.TokenType with
            | JsonTokenType.Number -> reader.GetDecimal() |> ValueSome
            | JsonTokenType.String -> reader.GetString() |> decimal |> ValueSome
            | _ ->
                reader.Skip()
                ValueNone
        | false -> ValueNone

    let readBoolean (reader: byref<Utf8JsonReader>) =
        match reader.Read() with
        | true ->
            match reader.TokenType with
            | JsonTokenType.True -> true |> ValueSome
            | JsonTokenType.False -> false |> ValueSome
            | JsonTokenType.Number -> reader.GetInt32() <> 0 |> ValueSome
            | _ ->
                reader.Skip()
                ValueNone
        | false -> ValueNone

    let readBytes (reader: byref<Utf8JsonReader>) =
        match reader.Read() with
        | true ->
            match reader.TokenType with
            | JsonTokenType.String -> reader.GetBytesFromBase64() |> ValueSome
            | _ ->
                reader.Skip()
                ValueNone
        | false -> ValueNone

    let writeGuid (writer: inref<Utf8JsonWriter>, value: System.Guid) =
        writer.WriteStringValue(value.ToString())

    let readGuid (reader: byref<Utf8JsonReader>) =
        match reader.Read() with
        | true ->
            match reader.TokenType with
            | JsonTokenType.String -> reader.GetString() |> System.Guid |> ValueSome
            | _ ->
                reader.Skip()
                ValueNone
        | false -> ValueNone


    let writeTimestamp (writer: inref<Utf8JsonWriter>, value: System.DateTime) =
        writer.WriteStringValue(fromDateTime value)

    let readTimestamp (reader: byref<Utf8JsonReader>) =
        match reader.Read() with
        | true ->
            match reader.TokenType with
            | JsonTokenType.String -> reader.GetDateTime() |> ValueSome
            | _ ->
                reader.Skip()
                ValueNone
        | false -> ValueNone

    let writeDuration (writer: inref<Utf8JsonWriter>, value: System.TimeSpan) =
        writer.WriteStringValue(fromTimeSpan value)

    let readDuration (reader: byref<Utf8JsonReader>) =
        readString &reader |> ValueOption.map toTimeSpan

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

    let readShort (reader: IO.IBsonReader) =
        match reader.CurrentBsonType with
        | BsonType.Int32 -> int16 (reader.ReadInt32()) |> ValueSome
        | BsonType.Int64 -> int16 (reader.ReadInt64()) |> ValueSome
        | BsonType.Double -> int16 (reader.ReadDouble()) |> ValueSome
        | BsonType.String -> int16 (reader.ReadString()) |> ValueSome
        | _ ->
            reader.SkipValue()
            ValueNone

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

