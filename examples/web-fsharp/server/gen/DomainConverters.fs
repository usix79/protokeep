namespace Domain.JsonConverters
open System.Text.Json
open Protokeep.FsharpJsonConvertersHelpers
type ConvertDomain () =
    static member OpFromJson (reader: byref<Utf8JsonReader>): Domain.Op =
        let mutable y = Domain.Op.Unknown
        if reader.TokenType = JsonTokenType.StartObject || reader.Read() && reader.TokenType = JsonTokenType.StartObject then
            while reader.Read() && reader.TokenType <> JsonTokenType.EndObject do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Val")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.Number
                    then y <- reader.GetInt32() |> Domain.Op.Val
                    else reader.Skip()
                else if (reader.ValueTextEquals("Sum")) then
                    y <- ConvertDomain.OpCaseSumFromJson(&reader)
                else if (reader.ValueTextEquals("Mul")) then
                    y <- ConvertDomain.OpCaseMulFromJson(&reader)
                else if (reader.ValueTextEquals("Div")) then
                    y <- ConvertDomain.OpCaseDivFromJson(&reader)
                else if (reader.ValueTextEquals("Ln")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.StartObject
                    then y <- ConvertDomain.OpFromJson(&reader) |> Domain.Op.Ln
                    else reader.Skip()
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
    static member OpCaseSumFromJson (reader: byref<Utf8JsonReader>) =
        let mutable p1 = Domain.Op.Unknown
        let mutable p2 = Domain.Op.Unknown
        if reader.TokenType = JsonTokenType.StartObject || reader.Read() && reader.TokenType = JsonTokenType.StartObject then
            while (reader.Read() && reader.TokenType <> JsonTokenType.EndObject) do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("P1")) then
                    p1 <- ConvertDomain.OpFromJson(&reader)
                else if (reader.ValueTextEquals("P2")) then
                    p2 <- ConvertDomain.OpFromJson(&reader)
                else reader.Skip()
        Domain.Op.Sum (p1,p2)
    static member OpCaseSumToJson (writer: inref<Utf8JsonWriter>,p1,p2) =
        writer.WriteStartObject()
        writer.WritePropertyName("P1")
        ConvertDomain.OpToJson(&writer, p1)
        writer.WritePropertyName("P2")
        ConvertDomain.OpToJson(&writer, p2)
        writer.WriteEndObject()
    static member OpCaseMulFromJson (reader: byref<Utf8JsonReader>) =
        let mutable p1 = Domain.Op.Unknown
        let mutable p2 = Domain.Op.Unknown
        if reader.TokenType = JsonTokenType.StartObject || reader.Read() && reader.TokenType = JsonTokenType.StartObject then
            while (reader.Read() && reader.TokenType <> JsonTokenType.EndObject) do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("P1")) then
                    p1 <- ConvertDomain.OpFromJson(&reader)
                else if (reader.ValueTextEquals("P2")) then
                    p2 <- ConvertDomain.OpFromJson(&reader)
                else reader.Skip()
        Domain.Op.Mul (p1,p2)
    static member OpCaseMulToJson (writer: inref<Utf8JsonWriter>,p1,p2) =
        writer.WriteStartObject()
        writer.WritePropertyName("P1")
        ConvertDomain.OpToJson(&writer, p1)
        writer.WritePropertyName("P2")
        ConvertDomain.OpToJson(&writer, p2)
        writer.WriteEndObject()
    static member OpCaseDivFromJson (reader: byref<Utf8JsonReader>) =
        let mutable p1 = Domain.Op.Unknown
        let mutable p2 = Domain.Op.Unknown
        if reader.TokenType = JsonTokenType.StartObject || reader.Read() && reader.TokenType = JsonTokenType.StartObject then
            while (reader.Read() && reader.TokenType <> JsonTokenType.EndObject) do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("P1")) then
                    p1 <- ConvertDomain.OpFromJson(&reader)
                else if (reader.ValueTextEquals("P2")) then
                    p2 <- ConvertDomain.OpFromJson(&reader)
                else reader.Skip()
        Domain.Op.Div (p1,p2)
    static member OpCaseDivToJson (writer: inref<Utf8JsonWriter>,p1,p2) =
        writer.WriteStartObject()
        writer.WritePropertyName("P1")
        ConvertDomain.OpToJson(&writer, p1)
        writer.WritePropertyName("P2")
        ConvertDomain.OpToJson(&writer, p2)
        writer.WriteEndObject()
    static member OpCaseQuantumFromJson (reader: byref<Utf8JsonReader>) =
        let mutable p1 = Domain.Op.Unknown
        let mutable p2 = Domain.Op.Unknown
        let mutable p3 = ""
        if reader.TokenType = JsonTokenType.StartObject || reader.Read() && reader.TokenType = JsonTokenType.StartObject then
            while (reader.Read() && reader.TokenType <> JsonTokenType.EndObject) do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("P1")) then
                    p1 <- ConvertDomain.OpFromJson(&reader)
                else if (reader.ValueTextEquals("P2")) then
                    p2 <- ConvertDomain.OpFromJson(&reader)
                else if (reader.ValueTextEquals("P3")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.String
                    then p3 <- reader.GetString()
                    else reader.Skip()
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
    static member OpCaseImagineFromJson (reader: byref<Utf8JsonReader>) =
        let mutable p1 = None
        if reader.TokenType = JsonTokenType.StartObject || reader.Read() && reader.TokenType = JsonTokenType.StartObject then
            while (reader.Read() && reader.TokenType <> JsonTokenType.EndObject) do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("P1Value")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.Number
                    then p1 <- reader.GetInt32() |> Some
                    else reader.Skip()
                else reader.Skip()
        Domain.Op.Imagine (p1)
    static member OpCaseImagineToJson (writer: inref<Utf8JsonWriter>,p1) =
        writer.WriteStartObject()
        match p1 with
        | Some v ->
            writer.WritePropertyName("P1Value")
            writer.WriteNumberValue(v)
        | None -> ()
        writer.WriteEndObject()
    static member OpErrorFromJson (reader: byref<Utf8JsonReader>): Domain.OpError =
        let mutable y = Domain.OpError.Unknown
        if reader.TokenType = JsonTokenType.StartObject || reader.Read() && reader.TokenType = JsonTokenType.StartObject then
            while reader.Read() && reader.TokenType <> JsonTokenType.EndObject do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("General")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.String
                    then y <- reader.GetString() |> Domain.OpError.General
                    else reader.Skip()
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
    static member OpResultFromJson (reader: byref<Utf8JsonReader>): Domain.OpResult =
        let mutable y = Domain.OpResult.Unknown
        if reader.TokenType = JsonTokenType.StartObject || reader.Read() && reader.TokenType = JsonTokenType.StartObject then
            while reader.Read() && reader.TokenType <> JsonTokenType.EndObject do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Success")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.Number
                    then y <- reader.GetInt32() |> Domain.OpResult.Success
                    else reader.Skip()
                else if (reader.ValueTextEquals("Fail")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.StartObject
                    then y <- ConvertDomain.OpErrorFromJson(&reader) |> Domain.OpResult.Fail
                    else reader.Skip()
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
    static member DefaultRequest: Lazy<Domain.Request> =
        lazy {
            Token = ""
            Operation = Domain.Op.Unknown
        }
    static member RequestFromJson (reader: byref<Utf8JsonReader>): Domain.Request =
        let mutable vToken = ""
        let mutable vOperation = Domain.Op.Unknown
        if reader.TokenType = JsonTokenType.StartObject || reader.Read() && reader.TokenType = JsonTokenType.StartObject then
            while (reader.Read() && reader.TokenType <> JsonTokenType.EndObject) do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Token")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.String
                    then vToken <- reader.GetString()
                    else reader.Skip()
                else if (reader.ValueTextEquals("Operation")) then
                    vOperation <- ConvertDomain.OpFromJson(&reader)
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
    static member DefaultResponse: Lazy<Domain.Response> =
        lazy {
            Token = ""
            Result = Domain.OpResult.Unknown
            ExecutionTime = System.TimeSpan.Zero
            Extra = None
            Since = System.DateTime.MinValue
            Tags = Map.empty
            Status = ConvertDomainSubdomain.DefaultStatus.Value
        }
    static member ResponseFromJson (reader: byref<Utf8JsonReader>): Domain.Response =
        let mutable vToken = ""
        let mutable vResult = Domain.OpResult.Unknown
        let mutable vExecutionTime = System.TimeSpan.Zero
        let mutable vExtra = None
        let mutable vSince = System.DateTime.MinValue
        let mutable vTags = ResizeArray()
        let mutable vStatus = ConvertDomainSubdomain.DefaultStatus.Value
        if reader.TokenType = JsonTokenType.StartObject || reader.Read() && reader.TokenType = JsonTokenType.StartObject then
            while (reader.Read() && reader.TokenType <> JsonTokenType.EndObject) do
                if reader.TokenType <> JsonTokenType.PropertyName then ()
                else if (reader.ValueTextEquals("Token")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.String
                    then vToken <- reader.GetString()
                    else reader.Skip()
                else if (reader.ValueTextEquals("Result")) then
                    vResult <- ConvertDomain.OpResultFromJson(&reader)
                else if (reader.ValueTextEquals("ExecutionTime")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.String
                    then vExecutionTime <- reader.GetString() |> toTimeSpan
                    else reader.Skip()
                else if (reader.ValueTextEquals("ExtraValue")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.String
                    then vExtra <- reader.GetString() |> Some
                    else reader.Skip()
                else if (reader.ValueTextEquals("Since")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.String
                    then vSince <- reader.GetDateTime()
                    else reader.Skip()
                else if (reader.ValueTextEquals("Tags")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.StartObject then
                        while reader.Read() && reader.TokenType <> JsonTokenType.EndObject do
                            let propName = reader.GetString()
                            if reader.Read() && reader.TokenType = JsonTokenType.String then
                                vTags.Add((propName, reader.GetString()))
                            else reader.Skip()
                    else reader.Skip()
                else if (reader.ValueTextEquals("Status")) then
                    if reader.Read() && reader.TokenType = JsonTokenType.String
                    then vStatus <- reader.GetString() |> ConvertDomainSubdomain.StatusFromString
                    else reader.Skip()
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
        writer.WriteStringValue(x.ExecutionTime |> fromTimeSpan)
        match x.Extra with
        | Some v ->
            writer.WritePropertyName("ExtraValue")
            writer.WriteStringValue(v)
        | None -> ()
        writer.WritePropertyName("Since")
        writer.WriteStringValue(x.Since |> fromDateTime)
        writer.WritePropertyName("Tags")
        writer.WriteStartObject(); (for pair in x.Tags do writer.WritePropertyName(pair.Key); writer.WriteStringValue(pair.Value)); writer.WriteEndObject()
        writer.WritePropertyName("Status")
        writer.WriteStringValue(x.Status |> ConvertDomainSubdomain.StatusToString)
        writer.WriteEndObject()
