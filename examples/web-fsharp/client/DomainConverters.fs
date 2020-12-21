namespace Protogen.FableConverters
open Fable.SimpleJson
open Protogen.FableConverterHelpers
type ConvertDomain () =
    static member OpFromJson (json: Json): Domain.Op =
        let mutable y = Domain.Op.Unknown
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Val" -> pair.Value |> ifNumber (fun v -> y <- v |> unbox |> Domain.Op.Val)
            | "Sum" -> pair.Value |> (fun v -> y <- v |> ConvertDomain.OpCaseSumFromJson)
            | "Mul" -> pair.Value |> (fun v -> y <- v |> ConvertDomain.OpCaseMulFromJson)
            | "Div" -> pair.Value |> (fun v -> y <- v |> ConvertDomain.OpCaseDivFromJson)
            | "Ln" -> pair.Value |> (fun v -> y <- v |> ConvertDomain.OpFromJson |> Domain.Op.Ln)
            | "Quantum" -> pair.Value |> (fun v -> y <- v |> ConvertDomain.OpCaseQuantumFromJson)
            | "Zero" -> pair.Value |> ifBool (fun v -> y <- Domain.Op.Zero)
            | _ -> () )
        y
    static member OpToJson (x:Domain.Op) =
        match x with
        | Domain.Op.Val (p1) -> "Val", JNumber (unbox p1)
        | Domain.Op.Sum (p1,p2) -> "Sum", ConvertDomain.OpCaseSumToJson (p1,p2)
        | Domain.Op.Mul (p1,p2) -> "Mul", ConvertDomain.OpCaseMulToJson (p1,p2)
        | Domain.Op.Div (p1,p2) -> "Div", ConvertDomain.OpCaseDivToJson (p1,p2)
        | Domain.Op.Ln (p1) -> "Ln", (p1 |> ConvertDomain.OpToJson)
        | Domain.Op.Quantum (p1,p2,p3) -> "Quantum", ConvertDomain.OpCaseQuantumToJson (p1,p2,p3)
        | Domain.Op.Zero -> "Zero", JBool (true)
        | _ -> "Unknown", JBool (true)
        |> List.singleton |> Map.ofList |> JObject
    static member OpCaseSumFromJson (json: Json) =
        let mutable p1 = Domain.Op.Unknown
        let mutable p2 = Domain.Op.Unknown
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "P1" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpFromJson)
            | "P2" -> pair.Value |> (fun v -> p2 <- v |> ConvertDomain.OpFromJson)
            | _ -> () )
        Domain.Op.Sum (p1,p2)
    static member OpCaseSumToJson (p1,p2) =
        [
           "P1", (p1 |> ConvertDomain.OpToJson)
           "P2", (p2 |> ConvertDomain.OpToJson)
        ] |> Map.ofList |> JObject
    static member OpCaseMulFromJson (json: Json) =
        let mutable p1 = Domain.Op.Unknown
        let mutable p2 = Domain.Op.Unknown
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "P1" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpFromJson)
            | "P2" -> pair.Value |> (fun v -> p2 <- v |> ConvertDomain.OpFromJson)
            | _ -> () )
        Domain.Op.Mul (p1,p2)
    static member OpCaseMulToJson (p1,p2) =
        [
           "P1", (p1 |> ConvertDomain.OpToJson)
           "P2", (p2 |> ConvertDomain.OpToJson)
        ] |> Map.ofList |> JObject
    static member OpCaseDivFromJson (json: Json) =
        let mutable p1 = Domain.Op.Unknown
        let mutable p2 = Domain.Op.Unknown
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "P1" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpFromJson)
            | "P2" -> pair.Value |> (fun v -> p2 <- v |> ConvertDomain.OpFromJson)
            | _ -> () )
        Domain.Op.Div (p1,p2)
    static member OpCaseDivToJson (p1,p2) =
        [
           "P1", (p1 |> ConvertDomain.OpToJson)
           "P2", (p2 |> ConvertDomain.OpToJson)
        ] |> Map.ofList |> JObject
    static member OpCaseLnFromJson (json: Json) =
        let mutable p1 = Domain.Op.Unknown
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "P1" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpFromJson)
            | _ -> () )
        Domain.Op.Ln (p1)
    static member OpCaseLnToJson (p1) =
        [
           "P1", (p1 |> ConvertDomain.OpToJson)
        ] |> Map.ofList |> JObject
    static member OpCaseQuantumFromJson (json: Json) =
        let mutable p1 = Domain.Op.Unknown
        let mutable p2 = Domain.Op.Unknown
        let mutable p3 = ""
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "P1" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpFromJson)
            | "P2" -> pair.Value |> (fun v -> p2 <- v |> ConvertDomain.OpFromJson)
            | "P3" -> pair.Value |> ifString (fun v -> p3 <- v)
            | _ -> () )
        Domain.Op.Quantum (p1,p2,p3)
    static member OpCaseQuantumToJson (p1,p2,p3) =
        [
           "P1", (p1 |> ConvertDomain.OpToJson)
           "P2", (p2 |> ConvertDomain.OpToJson)
           "P3", JString (p3)
        ] |> Map.ofList |> JObject
    static member OpErrorFromJson (json: Json): Domain.OpError =
        let mutable y = Domain.OpError.Unknown
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "General" -> pair.Value |> ifString (fun v -> y <- v |> Domain.OpError.General)
            | "DivisionByZero" -> pair.Value |> ifBool (fun v -> y <- Domain.OpError.DivisionByZero)
            | "NotSupported" -> pair.Value |> ifBool (fun v -> y <- Domain.OpError.NotSupported)
            | _ -> () )
        y
    static member OpErrorToJson (x:Domain.OpError) =
        match x with
        | Domain.OpError.General (p1) -> "General", JString (p1)
        | Domain.OpError.DivisionByZero -> "DivisionByZero", JBool (true)
        | Domain.OpError.NotSupported -> "NotSupported", JBool (true)
        | _ -> "Unknown", JBool (true)
        |> List.singleton |> Map.ofList |> JObject
    static member OpResultFromJson (json: Json): Domain.OpResult =
        let mutable y = Domain.OpResult.Unknown
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Success" -> pair.Value |> ifNumber (fun v -> y <- v |> unbox |> Domain.OpResult.Success)
            | "Fail" -> pair.Value |> (fun v -> y <- v |> ConvertDomain.OpErrorFromJson |> Domain.OpResult.Fail)
            | _ -> () )
        y
    static member OpResultToJson (x:Domain.OpResult) =
        match x with
        | Domain.OpResult.Success (p1) -> "Success", JNumber (unbox p1)
        | Domain.OpResult.Fail (p1) -> "Fail", (p1 |> ConvertDomain.OpErrorToJson)
        | _ -> "Unknown", JBool (true)
        |> List.singleton |> Map.ofList |> JObject
    static member OpResultCaseFailFromJson (json: Json) =
        let mutable p1 = Domain.OpError.Unknown
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "P1" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpErrorFromJson)
            | _ -> () )
        Domain.OpResult.Fail (p1)
    static member OpResultCaseFailToJson (p1) =
        [
           "P1", (p1 |> ConvertDomain.OpErrorToJson)
        ] |> Map.ofList |> JObject
    static member DefaultRequest: Lazy<Domain.Request> =
        lazy {
            Token = ""
            Operation = Domain.Op.Unknown
        }
    static member RequestFromJson (json: Json): Domain.Request =
        let mutable vToken = ""
        let mutable vOperation = Domain.Op.Unknown
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Token" -> pair.Value |> ifString (fun v -> vToken <- v)
            | "Operation" -> pair.Value |> (fun v -> vOperation <- v |> ConvertDomain.OpFromJson)
            | _ -> () )
        {
            Token = vToken
            Operation = vOperation
        }
    static member RequestToJson (x: Domain.Request) =
        [
           "Token", JString (x.Token)
           "Operation", (x.Operation |> ConvertDomain.OpToJson)
        ] |> Map.ofList |> JObject
    static member DefaultResponse: Lazy<Domain.Response> =
        lazy {
            Token = ""
            Result = Domain.OpResult.Unknown
            ExecutionTime = System.TimeSpan.Zero
            Extra = None
            Since = System.DateTimeOffset.MinValue
            Tags = Map.empty
        }
    static member ResponseFromJson (json: Json): Domain.Response =
        let mutable vToken = ""
        let mutable vResult = Domain.OpResult.Unknown
        let mutable vExecutionTime = System.TimeSpan.Zero
        let mutable vExtra = None
        let mutable vSince = System.DateTimeOffset.MinValue
        let mutable vTags = ResizeArray()
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Token" -> pair.Value |> ifString (fun v -> vToken <- v)
            | "Result" -> pair.Value |> (fun v -> vResult <- v |> ConvertDomain.OpResultFromJson)
            | "ExecutionTime" -> pair.Value |> ifString (fun v -> vExecutionTime <- v |> toTimeSpan)
            | "ExtraValue" -> pair.Value |> ifString (fun v -> vExtra <- v |> Some)
            | "Since" -> pair.Value |> ifString (fun v -> vSince <- v |> toDateTimeOffset)
            | "Tags" -> pair.Value |> ifObject (Map.iter (fun key -> ifString (fun v -> v |> fun v -> vTags.Add(key, v))))
            | _ -> () )
        {
            Token = vToken
            Result = vResult
            ExecutionTime = vExecutionTime
            Extra = vExtra
            Since = vSince
            Tags = vTags |> Map.ofSeq
        }
    static member ResponseToJson (x: Domain.Response) =
        [
           "Token", JString (x.Token)
           "Result", (x.Result |> ConvertDomain.OpResultToJson)
           "ExecutionTime", JString (x.ExecutionTime |> fromTimeSpan)
           match x.Extra with
           | Some v -> "ExtraValue", JString (v)
           | None -> ()
           "Since", JString (x.Since |> fromDateTimeOffset)
           "Tags", JObject (x.Tags |> Map.map (fun _ v -> JString (v)))
        ] |> Map.ofList |> JObject
