namespace Domain.JsonConverters

open System.Text.Json
open Protokeep

type ConvertDomain() =

    static member OpFromJson(reader: byref<Utf8JsonReader>): Domain.Op =
        let mutable y = Domain.Op.Unknown
        if FsharpJsonHelpers.moveToStartObject(&reader)then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Val")) then
                    match FsharpJsonHelpers.readInt(&reader) with
                    | ValueSome v -> y <- v |> Domain.Op.Val
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Sum")) then
                    y <- ConvertDomain.OpCaseSumFromJson(&reader)
                else if (reader.ValueTextEquals("Mul")) then
                    y <- ConvertDomain.OpCaseMulFromJson(&reader)
                else if (reader.ValueTextEquals("Div")) then
                    y <- ConvertDomain.OpCaseDivFromJson(&reader)
                else if (reader.ValueTextEquals("Ln")) then
                    y <- ConvertDomain.OpFromJson(&reader) |> Domain.Op.Ln
                else if (reader.ValueTextEquals("Quantum")) then
                    y <- ConvertDomain.OpCaseQuantumFromJson(&reader)
                else if (reader.ValueTextEquals("Imagine")) then
                    y <- ConvertDomain.OpCaseImagineFromJson(&reader)
                else if (reader.ValueTextEquals("Zero")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.True
                    then y <- Domain.Op.Zero
                    else reader.Skip()
                else reader.Skip()
        y
    static member OpToJson (writer:inref<Utf8JsonWriter>, x: Domain.Op) =
        writer.WriteStartObject()
        match x with
        | Domain.Op.Val (p1) ->
            writer.WritePropertyName("Val")
            writer.WriteNumberValue(p1)
        | Domain.Op.Sum (p1,p2) ->
            writer.WritePropertyName("Sum")
            ConvertDomain.OpCaseSumToJson(&writer,p1,p2)
        | Domain.Op.Mul (p1,p2) ->
            writer.WritePropertyName("Mul")
            ConvertDomain.OpCaseMulToJson(&writer,p1,p2)
        | Domain.Op.Div (p1,p2) ->
            writer.WritePropertyName("Div")
            ConvertDomain.OpCaseDivToJson(&writer,p1,p2)
        | Domain.Op.Ln (p1) ->
            writer.WritePropertyName("Ln")
            ConvertDomain.OpToJson(&writer, p1)
        | Domain.Op.Quantum (p1,p2,p3) ->
            writer.WritePropertyName("Quantum")
            ConvertDomain.OpCaseQuantumToJson(&writer,p1,p2,p3)
        | Domain.Op.Imagine (p1) ->
            writer.WritePropertyName("Imagine")
            ConvertDomain.OpCaseImagineToJson(&writer,p1)
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
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("P1")) then
                    match ConvertDomain.OpFromJson(&reader) |> ValueSome with
                    | ValueSome v -> p1 <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("P2")) then
                    match ConvertDomain.OpFromJson(&reader) |> ValueSome with
                    | ValueSome v -> p2 <- v
                    | ValueNone -> ()
                else reader.Skip()
        Domain.Op.Sum (p1,p2)
    static member OpCaseSumToJson (writer: inref<Utf8JsonWriter>,p1,p2) =
        writer.WriteStartObject()
        writer.WritePropertyName("P1")
        ConvertDomain.OpToJson(&writer, p1)
        writer.WritePropertyName("P2")
        ConvertDomain.OpToJson(&writer, p2)
        writer.WriteEndObject()
    static member OpCaseMulFromJson(reader: byref<Utf8JsonReader>) =
        let mutable p1 = Domain.Op.Unknown
        let mutable p2 = Domain.Op.Unknown
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("P1")) then
                    match ConvertDomain.OpFromJson(&reader) |> ValueSome with
                    | ValueSome v -> p1 <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("P2")) then
                    match ConvertDomain.OpFromJson(&reader) |> ValueSome with
                    | ValueSome v -> p2 <- v
                    | ValueNone -> ()
                else reader.Skip()
        Domain.Op.Mul (p1,p2)
    static member OpCaseMulToJson (writer: inref<Utf8JsonWriter>,p1,p2) =
        writer.WriteStartObject()
        writer.WritePropertyName("P1")
        ConvertDomain.OpToJson(&writer, p1)
        writer.WritePropertyName("P2")
        ConvertDomain.OpToJson(&writer, p2)
        writer.WriteEndObject()
    static member OpCaseDivFromJson(reader: byref<Utf8JsonReader>) =
        let mutable p1 = Domain.Op.Unknown
        let mutable p2 = Domain.Op.Unknown
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("P1")) then
                    match ConvertDomain.OpFromJson(&reader) |> ValueSome with
                    | ValueSome v -> p1 <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("P2")) then
                    match ConvertDomain.OpFromJson(&reader) |> ValueSome with
                    | ValueSome v -> p2 <- v
                    | ValueNone -> ()
                else reader.Skip()
        Domain.Op.Div (p1,p2)
    static member OpCaseDivToJson (writer: inref<Utf8JsonWriter>,p1,p2) =
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
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("P1")) then
                    match ConvertDomain.OpFromJson(&reader) |> ValueSome with
                    | ValueSome v -> p1 <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("P2")) then
                    match ConvertDomain.OpFromJson(&reader) |> ValueSome with
                    | ValueSome v -> p2 <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("P3")) then
                    match FsharpJsonHelpers.readString(&reader) with
                    | ValueSome v -> p3 <- v
                    | ValueNone -> ()
                else reader.Skip()
        Domain.Op.Quantum (p1,p2,p3)
    static member OpCaseQuantumToJson (writer: inref<Utf8JsonWriter>,p1,p2,p3) =
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
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("P1Value")) then
                    match FsharpJsonHelpers.readInt(&reader) |> ValueOption.map ValueSome with
                    | ValueSome v -> p1 <- v
                    | ValueNone -> ()
                else reader.Skip()
        Domain.Op.Imagine (p1)
    static member OpCaseImagineToJson (writer: inref<Utf8JsonWriter>,p1) =
        writer.WriteStartObject()
        match p1 with
        | ValueSome v ->
            writer.WritePropertyName("P1Value")
            writer.WriteNumberValue(v)
        | ValueNone -> ()
        writer.WriteEndObject()

    static member OpErrorFromJson(reader: byref<Utf8JsonReader>): Domain.OpError =
        let mutable y = Domain.OpError.Unknown
        if FsharpJsonHelpers.moveToStartObject(&reader)then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("General")) then
                    match FsharpJsonHelpers.readString(&reader) with
                    | ValueSome v -> y <- v |> Domain.OpError.General
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("DivisionByZero")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.True
                    then y <- Domain.OpError.DivisionByZero
                    else reader.Skip()
                else if (reader.ValueTextEquals("NotSupported")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.True
                    then y <- Domain.OpError.NotSupported
                    else reader.Skip()
                else reader.Skip()
        y
    static member OpErrorToJson (writer:inref<Utf8JsonWriter>, x: Domain.OpError) =
        writer.WriteStartObject()
        match x with
        | Domain.OpError.General (p1) ->
            writer.WritePropertyName("General")
            writer.WriteStringValue(p1)
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

    static member OpResultFromJson(reader: byref<Utf8JsonReader>): Domain.OpResult =
        let mutable y = Domain.OpResult.Unknown
        if FsharpJsonHelpers.moveToStartObject(&reader)then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Success")) then
                    match FsharpJsonHelpers.readInt(&reader) with
                    | ValueSome v -> y <- v |> Domain.OpResult.Success
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Fail")) then
                    y <- ConvertDomain.OpErrorFromJson(&reader) |> Domain.OpResult.Fail
                else reader.Skip()
        y
    static member OpResultToJson (writer:inref<Utf8JsonWriter>, x: Domain.OpResult) =
        writer.WriteStartObject()
        match x with
        | Domain.OpResult.Success (p1) ->
            writer.WritePropertyName("Success")
            writer.WriteNumberValue(p1)
        | Domain.OpResult.Fail (p1) ->
            writer.WritePropertyName("Fail")
            ConvertDomain.OpErrorToJson(&writer, p1)
        | _ ->
            writer.WritePropertyName("Unknown")
            writer.WriteBooleanValue(true)
        writer.WriteEndObject()

    static member RequestFromJson(reader: byref<Utf8JsonReader>): Domain.Request =
        let mutable vToken = ""
        let mutable vOperation = Domain.Op.Unknown
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Token")) then
                    match FsharpJsonHelpers.readString(&reader) with
                    | ValueSome v -> vToken <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Operation")) then
                    match ConvertDomain.OpFromJson(&reader) |> ValueSome with
                    | ValueSome v -> vOperation <- v
                    | ValueNone -> ()
                else reader.Skip()
        {
            Token = vToken
            Operation = vOperation
        }
    static member RequestToJson (writer: inref<Utf8JsonWriter>, x: Domain.Request) =
        writer.WriteStartObject()
        writer.WritePropertyName("Token")
        writer.WriteStringValue(x.Token)
        writer.WritePropertyName("Operation")
        ConvertDomain.OpToJson(&writer, x.Operation)
        writer.WriteEndObject()

    static member ResponseFromJson(reader: byref<Utf8JsonReader>): Domain.Response =
        let mutable vToken = ""
        let mutable vResult = Domain.OpResult.Unknown
        let mutable vExecutionTime = System.TimeSpan.Zero
        let mutable vExtra = ValueNone
        let mutable vSince = System.DateTime.MinValue
        let mutable vTags = ResizeArray()
        let mutable vStatus = Domain.Subdomain.Status.Unknown
        if FsharpJsonHelpers.moveToStartObject(&reader) then
            while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Token")) then
                    match FsharpJsonHelpers.readString(&reader) with
                    | ValueSome v -> vToken <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Result")) then
                    match ConvertDomain.OpResultFromJson(&reader) |> ValueSome with
                    | ValueSome v -> vResult <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("ExecutionTime")) then
                    match FsharpJsonHelpers.readDuration(&reader) with
                    | ValueSome v -> vExecutionTime <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("ExtraValue")) then
                    match FsharpJsonHelpers.readString(&reader) |> ValueOption.map ValueSome with
                    | ValueSome v -> vExtra <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Since")) then
                    match FsharpJsonHelpers.readTimestamp(&reader) with
                    | ValueSome v -> vSince <- v
                    | ValueNone -> ()
                else if (reader.ValueTextEquals("Tags")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.StartObject then
                        while FsharpJsonHelpers.moveToEndObject(&reader) = false do
                            let propName = reader.GetString()
                            match FsharpJsonHelpers.readString(&reader) with
                            | ValueSome v -> vTags.Add(propName, v)
                            | ValueNone -> ()
                    else reader.Skip()
                else if (reader.ValueTextEquals("Status")) then
                    match FsharpJsonHelpers.readString(&reader) |> ValueOption.map ConvertDomainSubdomain.StatusFromString with
                    | ValueSome v -> vStatus <- v
                    | ValueNone -> ()
                else reader.Skip()
        {
            Token = vToken
            Result = vResult
            ExecutionTime = vExecutionTime
            Extra = vExtra
            Since = vSince
            Tags = vTags |> Map.ofSeq
            Status = vStatus
        }
    static member ResponseToJson (writer: inref<Utf8JsonWriter>, x: Domain.Response) =
        writer.WriteStartObject()
        writer.WritePropertyName("Token")
        writer.WriteStringValue(x.Token)
        writer.WritePropertyName("Result")
        ConvertDomain.OpResultToJson(&writer, x.Result)
        writer.WritePropertyName("ExecutionTime")
        FsharpJsonHelpers.writeDuration(&writer, x.ExecutionTime)
        match x.Extra with
        | ValueSome v ->
            writer.WritePropertyName("ExtraValue")
            writer.WriteStringValue(v)
        | ValueNone -> ()
        writer.WritePropertyName("Since")
        FsharpJsonHelpers.writeTimestamp(&writer, x.Since)
        writer.WritePropertyName("Tags")
        writer.WriteStartObject(); (for pair in x.Tags do writer.WritePropertyName(pair.Key); writer.WriteStringValue(pair.Value)); writer.WriteEndObject()
        writer.WritePropertyName("Status")
        writer.WriteStringValue(x.Status |> ConvertDomainSubdomain.StatusToString)
        writer.WriteEndObject()

