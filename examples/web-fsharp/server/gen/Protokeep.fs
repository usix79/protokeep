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

