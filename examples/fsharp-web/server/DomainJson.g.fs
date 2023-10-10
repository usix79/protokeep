namespace Domain.JsonConverters

open System.Text.Json
open Protokeep

type ConvertDomain() =

    static member OpFromJson(reader: byref<Utf8JsonReader>) : Domain.Op voption =
        if FsharpJsonHelpers.moveToStartObject (&reader) then
            let mutable y = Domain.Op.Unknown

            while FsharpJsonHelpers.moveToEndObject (&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then
                    ()
                else if (reader.ValueTextEquals("Val")) then
                    let mutable _p1 = 0

                    match FsharpJsonHelpers.readInt (&reader) with
                    | ValueSome v -> _p1 <- v
                    | ValueNone -> ()

                    y <- _p1 |> Domain.Op.Val
                else if (reader.ValueTextEquals("Sum")) then
                    y <- ConvertDomain.OpCaseSumFromJson(&reader)
                else if (reader.ValueTextEquals("Mul")) then
                    y <- ConvertDomain.OpCaseMulFromJson(&reader)
                else if (reader.ValueTextEquals("Div")) then
                    y <- ConvertDomain.OpCaseDivFromJson(&reader)
                else if (reader.ValueTextEquals("Ln")) then
                    let mutable _p1 = Domain.Op.Unknown

                    match ConvertDomain.OpFromJson(&reader) with
                    | ValueSome v -> _p1 <- v
                    | ValueNone -> ()

                    y <- _p1 |> Domain.Op.Ln
                else if (reader.ValueTextEquals("Quantum")) then
                    y <- ConvertDomain.OpCaseQuantumFromJson(&reader)
                else if (reader.ValueTextEquals("Imagine")) then
                    y <- ConvertDomain.OpCaseImagineFromJson(&reader)
                else if (reader.ValueTextEquals("SumAll")) then
                    let mutable _p1 = ResizeArray()

                    if reader.Read() && reader.TokenType = JsonTokenType.StartArray then
                        while reader.TokenType <> JsonTokenType.EndArray do
                            match ConvertDomain.OpFromJson(&reader) with
                            | ValueSome v -> _p1.Add(v)
                            | ValueNone -> ()
                    else
                        reader.Skip()

                    y <- _p1 |> List.ofSeq |> Domain.Op.SumAll
                else if (reader.ValueTextEquals("Zero")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.True then
                        y <- Domain.Op.Zero
                    else
                        reader.Skip()
                else
                    reader.Skip()

            ValueSome y
        else
            ValueNone

    static member OpToJson(writer: inref<Utf8JsonWriter>, x: Domain.Op) =
        writer.WriteStartObject()

        match x with
        | Domain.Op.Val(p1) ->
            writer.WritePropertyName("Val")
            writer.WriteNumberValue(p1)
        | Domain.Op.Sum(p1, p2) ->
            writer.WritePropertyName("Sum")
            ConvertDomain.OpCaseSumToJson(&writer, p1, p2)
        | Domain.Op.Mul(p1, p2) ->
            writer.WritePropertyName("Mul")
            ConvertDomain.OpCaseMulToJson(&writer, p1, p2)
        | Domain.Op.Div(p1, p2) ->
            writer.WritePropertyName("Div")
            ConvertDomain.OpCaseDivToJson(&writer, p1, p2)
        | Domain.Op.Ln(p1) ->
            writer.WritePropertyName("Ln")
            ConvertDomain.OpToJson(&writer, p1)
        | Domain.Op.Quantum(p1, p2, p3) ->
            writer.WritePropertyName("Quantum")
            ConvertDomain.OpCaseQuantumToJson(&writer, p1, p2, p3)
        | Domain.Op.Imagine(p1) ->
            writer.WritePropertyName("Imagine")
            ConvertDomain.OpCaseImagineToJson(&writer, p1)
        | Domain.Op.SumAll(p1) ->
            writer.WritePropertyName("SumAll")
            writer.WriteStartArray()

            for v in p1 do
                ConvertDomain.OpToJson(&writer, v)

            writer.WriteEndArray()
        | Domain.Op.Zero ->
            writer.WritePropertyName("Zero")
            writer.WriteBooleanValue(true)
        | _ ->
            writer.WritePropertyName("Unknown")
            writer.WriteBooleanValue(true)

        writer.WriteEndObject()

    static member OpCaseSumFromJson(reader: byref<Utf8JsonReader>) =
        let mutable p1 = Domain.Op.Unknown
        let mutable p2 = Domain.Op.Unknown

        if FsharpJsonHelpers.moveToStartObject (&reader) then
            while FsharpJsonHelpers.moveToEndObject (&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then
                    ()
                else if (reader.ValueTextEquals("P1")) then
                    match ConvertDomain.OpFromJson(&reader) with
                    | ValueSome v -> p1 <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("P2")) then
                    match ConvertDomain.OpFromJson(&reader) with
                    | ValueSome v -> p2 <- v
                    | ValueNone -> ()
                else
                    reader.Skip()

        Domain.Op.Sum(p1, p2)

    static member OpCaseSumToJson(writer: inref<Utf8JsonWriter>, p1, p2) =
        writer.WriteStartObject()
        writer.WritePropertyName("P1")
        ConvertDomain.OpToJson(&writer, p1)
        writer.WritePropertyName("P2")
        ConvertDomain.OpToJson(&writer, p2)
        writer.WriteEndObject()

    static member OpCaseMulFromJson(reader: byref<Utf8JsonReader>) =
        let mutable p1 = Domain.Op.Unknown
        let mutable p2 = Domain.Op.Unknown

        if FsharpJsonHelpers.moveToStartObject (&reader) then
            while FsharpJsonHelpers.moveToEndObject (&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then
                    ()
                else if (reader.ValueTextEquals("P1")) then
                    match ConvertDomain.OpFromJson(&reader) with
                    | ValueSome v -> p1 <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("P2")) then
                    match ConvertDomain.OpFromJson(&reader) with
                    | ValueSome v -> p2 <- v
                    | ValueNone -> ()
                else
                    reader.Skip()

        Domain.Op.Mul(p1, p2)

    static member OpCaseMulToJson(writer: inref<Utf8JsonWriter>, p1, p2) =
        writer.WriteStartObject()
        writer.WritePropertyName("P1")
        ConvertDomain.OpToJson(&writer, p1)
        writer.WritePropertyName("P2")
        ConvertDomain.OpToJson(&writer, p2)
        writer.WriteEndObject()

    static member OpCaseDivFromJson(reader: byref<Utf8JsonReader>) =
        let mutable p1 = Domain.Op.Unknown
        let mutable p2 = Domain.Op.Unknown

        if FsharpJsonHelpers.moveToStartObject (&reader) then
            while FsharpJsonHelpers.moveToEndObject (&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then
                    ()
                else if (reader.ValueTextEquals("P1")) then
                    match ConvertDomain.OpFromJson(&reader) with
                    | ValueSome v -> p1 <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("P2")) then
                    match ConvertDomain.OpFromJson(&reader) with
                    | ValueSome v -> p2 <- v
                    | ValueNone -> ()
                else
                    reader.Skip()

        Domain.Op.Div(p1, p2)

    static member OpCaseDivToJson(writer: inref<Utf8JsonWriter>, p1, p2) =
        writer.WriteStartObject()
        writer.WritePropertyName("P1")
        ConvertDomain.OpToJson(&writer, p1)
        writer.WritePropertyName("P2")
        ConvertDomain.OpToJson(&writer, p2)
        writer.WriteEndObject()

    static member OpCaseQuantumFromJson(reader: byref<Utf8JsonReader>) =
        let mutable p1 = Domain.Op.Unknown
        let mutable p2 = Domain.Op.Unknown
        let mutable p3 = ""

        if FsharpJsonHelpers.moveToStartObject (&reader) then
            while FsharpJsonHelpers.moveToEndObject (&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then
                    ()
                else if (reader.ValueTextEquals("P1")) then
                    match ConvertDomain.OpFromJson(&reader) with
                    | ValueSome v -> p1 <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("P2")) then
                    match ConvertDomain.OpFromJson(&reader) with
                    | ValueSome v -> p2 <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("P3")) then
                    match FsharpJsonHelpers.readString (&reader) with
                    | ValueSome v -> p3 <- v
                    | ValueNone -> ()
                else
                    reader.Skip()

        Domain.Op.Quantum(p1, p2, p3)

    static member OpCaseQuantumToJson(writer: inref<Utf8JsonWriter>, p1, p2, p3) =
        writer.WriteStartObject()
        writer.WritePropertyName("P1")
        ConvertDomain.OpToJson(&writer, p1)
        writer.WritePropertyName("P2")
        ConvertDomain.OpToJson(&writer, p2)
        writer.WritePropertyName("P3")
        writer.WriteStringValue(p3)
        writer.WriteEndObject()

    static member OpCaseImagineFromJson(reader: byref<Utf8JsonReader>) =
        let mutable p1 = ValueNone

        if FsharpJsonHelpers.moveToStartObject (&reader) then
            while FsharpJsonHelpers.moveToEndObject (&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then
                    ()
                else if (reader.ValueTextEquals("P1Value")) then
                    match FsharpJsonHelpers.readInt (&reader) |> ValueOption.map ValueSome with
                    | ValueSome v -> p1 <- v
                    | ValueNone -> ()
                else
                    reader.Skip()

        Domain.Op.Imagine(p1)

    static member OpCaseImagineToJson(writer: inref<Utf8JsonWriter>, p1) =
        writer.WriteStartObject()

        match p1 with
        | ValueSome v ->
            writer.WritePropertyName("P1Value")
            writer.WriteNumberValue(v)
        | ValueNone -> ()

        writer.WriteEndObject()

    static member OpErrorFromJson(reader: byref<Utf8JsonReader>) : Domain.OpError voption =
        if FsharpJsonHelpers.moveToStartObject (&reader) then
            let mutable y = Domain.OpError.Unknown

            while FsharpJsonHelpers.moveToEndObject (&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then
                    ()
                else if (reader.ValueTextEquals("General")) then
                    let mutable _p1 = ResizeArray()

                    if reader.Read() && reader.TokenType = JsonTokenType.StartArray then
                        while reader.TokenType <> JsonTokenType.EndArray do
                            match FsharpJsonHelpers.readString (&reader) with
                            | ValueSome v -> _p1.Add(v)
                            | ValueNone -> ()
                    else
                        reader.Skip()

                    y <- _p1 |> List.ofSeq |> Domain.OpError.General
                else if (reader.ValueTextEquals("DivisionByZero")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.True then
                        y <- Domain.OpError.DivisionByZero
                    else
                        reader.Skip()
                else if (reader.ValueTextEquals("NotSupported")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.True then
                        y <- Domain.OpError.NotSupported
                    else
                        reader.Skip()
                else
                    reader.Skip()

            ValueSome y
        else
            ValueNone

    static member OpErrorToJson(writer: inref<Utf8JsonWriter>, x: Domain.OpError) =
        writer.WriteStartObject()

        match x with
        | Domain.OpError.General(p1) ->
            writer.WritePropertyName("General")
            writer.WriteStartArray()

            for v in p1 do
                writer.WriteStringValue(v)

            writer.WriteEndArray()
        | Domain.OpError.DivisionByZero ->
            writer.WritePropertyName("DivisionByZero")
            writer.WriteBooleanValue(true)
        | Domain.OpError.NotSupported ->
            writer.WritePropertyName("NotSupported")
            writer.WriteBooleanValue(true)
        | _ ->
            writer.WritePropertyName("Unknown")
            writer.WriteBooleanValue(true)

        writer.WriteEndObject()

    static member OpResultFromJson(reader: byref<Utf8JsonReader>) : Domain.OpResult voption =
        if FsharpJsonHelpers.moveToStartObject (&reader) then
            let mutable y = Domain.OpResult.Unknown

            while FsharpJsonHelpers.moveToEndObject (&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then
                    ()
                else if (reader.ValueTextEquals("Success")) then
                    let mutable _p1 = 0

                    match FsharpJsonHelpers.readInt (&reader) with
                    | ValueSome v -> _p1 <- v
                    | ValueNone -> ()

                    y <- _p1 |> Domain.OpResult.Success
                else if (reader.ValueTextEquals("Fail")) then
                    let mutable _p1 = Domain.OpError.Unknown

                    match ConvertDomain.OpErrorFromJson(&reader) with
                    | ValueSome v -> _p1 <- v
                    | ValueNone -> ()

                    y <- _p1 |> Domain.OpResult.Fail
                else
                    reader.Skip()

            ValueSome y
        else
            ValueNone

    static member OpResultToJson(writer: inref<Utf8JsonWriter>, x: Domain.OpResult) =
        writer.WriteStartObject()

        match x with
        | Domain.OpResult.Success(p1) ->
            writer.WritePropertyName("Success")
            writer.WriteNumberValue(p1)
        | Domain.OpResult.Fail(p1) ->
            writer.WritePropertyName("Fail")
            ConvertDomain.OpErrorToJson(&writer, p1)
        | _ ->
            writer.WritePropertyName("Unknown")
            writer.WriteBooleanValue(true)

        writer.WriteEndObject()

    static member RequestToJson(writer: inref<Utf8JsonWriter>, x: Domain.Request) =
        writer.WriteStartObject()
        writer.WritePropertyName("Token")
        writer.WriteStringValue(x.Token)
        writer.WritePropertyName("Operation")
        ConvertDomain.OpToJson(&writer, x.Operation)
        writer.WritePropertyName("Tags")
        writer.WriteStartArray()

        for pair in x.Tags do
            writer.WriteStartObject()
            writer.WritePropertyName("Key")
            writer.WriteStringValue(pair.Key)
            writer.WritePropertyName("Value")
            writer.WriteStringValue(pair.Value)
            writer.WriteEndObject()

        writer.WriteEndArray()
        writer.WriteEndObject()

    static member RequestFromJson(reader: byref<Utf8JsonReader>) : Domain.Request =
        let mutable vToken = ""
        let mutable vOperation = Domain.Op.Unknown
        let mutable vTags = ResizeArray()

        if FsharpJsonHelpers.moveToStartObject (&reader) then
            while FsharpJsonHelpers.moveToEndObject (&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then
                    ()
                else if (reader.ValueTextEquals("Token")) then
                    match FsharpJsonHelpers.readString (&reader) with
                    | ValueSome v -> vToken <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Operation")) then
                    match ConvertDomain.OpFromJson(&reader) with
                    | ValueSome v -> vOperation <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Tags")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.StartArray then
                        while reader.Read() && reader.TokenType <> JsonTokenType.EndArray do
                            if reader.TokenType = JsonTokenType.StartObject then
                                let mutable key = ""
                                let mutable value = ""

                                while reader.Read() && reader.TokenType <> JsonTokenType.EndObject do
                                    if (reader.ValueTextEquals("Key")) then
                                        match FsharpJsonHelpers.readString (&reader) with
                                        | ValueSome v -> key <- v
                                        | ValueNone -> ()
                                    else if (reader.ValueTextEquals("Value")) then
                                        match FsharpJsonHelpers.readString (&reader) with
                                        | ValueSome v -> value <- v
                                        | ValueNone -> ()
                                    else
                                        reader.Skip()

                                vTags.Add(key, value)
                            else
                                reader.Skip()
                    else
                        reader.Skip()
                else
                    reader.Skip()

        { Token = vToken
          Operation = vOperation
          Tags = vTags |> Map.ofSeq }

    static member ResponseToJson(writer: inref<Utf8JsonWriter>, x: Domain.Response) =
        writer.WriteStartObject()
        writer.WritePropertyName("Token")
        writer.WriteStringValue(x.Token)
        writer.WritePropertyName("Result")
        ConvertDomain.OpResultToJson(&writer, x.Result)
        writer.WritePropertyName("ExecutionTime")
        FsharpJsonHelpers.writeDuration (&writer, x.ExecutionTime)

        match x.Extra with
        | ValueSome v ->
            writer.WritePropertyName("ExtraValue")
            writer.WriteStringValue(v)
        | ValueNone -> ()

        writer.WritePropertyName("Since")
        FsharpJsonHelpers.writeTimestamp (&writer, x.Since)
        writer.WritePropertyName("Tags")
        writer.WriteStartArray()

        for pair in x.Tags do
            writer.WriteStartObject()
            writer.WritePropertyName("Key")
            writer.WriteStringValue(pair.Key)
            writer.WritePropertyName("Value")
            writer.WriteStringValue(pair.Value)
            writer.WriteEndObject()

        writer.WriteEndArray()
        writer.WritePropertyName("Status")
        writer.WriteStringValue(x.Status |> ConvertDomainSubdomain.StatusToString)
        writer.WriteEndObject()

    static member ResponseFromJson(reader: byref<Utf8JsonReader>) : Domain.Response =
        let mutable vToken = ""
        let mutable vResult = Domain.OpResult.Unknown
        let mutable vExecutionTime = System.TimeSpan.Zero
        let mutable vExtra = ValueNone
        let mutable vSince = System.DateTime.MinValue
        let mutable vTags = ResizeArray()
        let mutable vStatus = Domain.Subdomain.Status.Unknown

        if FsharpJsonHelpers.moveToStartObject (&reader) then
            while FsharpJsonHelpers.moveToEndObject (&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then
                    ()
                else if (reader.ValueTextEquals("Token")) then
                    match FsharpJsonHelpers.readString (&reader) with
                    | ValueSome v -> vToken <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Result")) then
                    match ConvertDomain.OpResultFromJson(&reader) with
                    | ValueSome v -> vResult <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("ExecutionTime")) then
                    match FsharpJsonHelpers.readDuration (&reader) with
                    | ValueSome v -> vExecutionTime <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("ExtraValue")) then
                    match FsharpJsonHelpers.readString (&reader) |> ValueOption.map ValueSome with
                    | ValueSome v -> vExtra <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Since")) then
                    match FsharpJsonHelpers.readTimestamp (&reader) with
                    | ValueSome v -> vSince <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Tags")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.StartArray then
                        while reader.Read() && reader.TokenType <> JsonTokenType.EndArray do
                            if reader.TokenType = JsonTokenType.StartObject then
                                let mutable key = ""
                                let mutable value = ""

                                while reader.Read() && reader.TokenType <> JsonTokenType.EndObject do
                                    if (reader.ValueTextEquals("Key")) then
                                        match FsharpJsonHelpers.readString (&reader) with
                                        | ValueSome v -> key <- v
                                        | ValueNone -> ()
                                    else if (reader.ValueTextEquals("Value")) then
                                        match FsharpJsonHelpers.readString (&reader) with
                                        | ValueSome v -> value <- v
                                        | ValueNone -> ()
                                    else
                                        reader.Skip()

                                vTags.Add(key, value)
                            else
                                reader.Skip()
                    else
                        reader.Skip()
                else if (reader.ValueTextEquals("Status")) then
                    match
                        FsharpJsonHelpers.readString (&reader)
                        |> ValueOption.map ConvertDomainSubdomain.StatusFromString
                    with
                    | ValueSome v -> vStatus <- v
                    | ValueNone -> ()
                else
                    reader.Skip()

        { Token = vToken
          Result = vResult
          ExecutionTime = vExecutionTime
          Extra = vExtra
          Since = vSince
          Tags = vTags |> Map.ofSeq
          Status = vStatus }
