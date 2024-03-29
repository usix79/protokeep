namespace Domain.JsonConverters

open Fable.SimpleJson
open Protokeep

type ConvertDomain () =
    static member OpFromJson (json: Json): Domain.Op =
        let mutable y = Domain.Op.Unknown
        FsharpFableHelpers.getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Val" ->
                let mutable _p1 = 0
                pair.Value |> FsharpFableHelpers.ifNumber (fun v -> _p1 <- v |> unbox)
                y <- _p1 |> Domain.Op.Val
            | "Sum" -> pair.Value |> (fun v -> y <- v |> ConvertDomain.OpCaseSumFromJson)
            | "Mul" -> pair.Value |> (fun v -> y <- v |> ConvertDomain.OpCaseMulFromJson)
            | "Div" -> pair.Value |> (fun v -> y <- v |> ConvertDomain.OpCaseDivFromJson)
            | "Ln" ->
                let mutable _p1 = Domain.Op.Unknown
                pair.Value |> (fun v -> _p1 <- v |> ConvertDomain.OpFromJson)
                y <- _p1 |> Domain.Op.Ln
            | "Quantum" -> pair.Value |> (fun v -> y <- v |> ConvertDomain.OpCaseQuantumFromJson)
            | "Imagine" -> pair.Value |> (fun v -> y <- v |> ConvertDomain.OpCaseImagineFromJson)
            | "SumAll" ->
                let mutable _p1 = ResizeArray()
                pair.Value |> FsharpFableHelpers.ifArray (Seq.iter ((fun v -> v |> ConvertDomain.OpFromJson |> _p1.Add)))
                y <- _p1 |> List.ofSeq |> Domain.Op.SumAll
            | "Zero" -> pair.Value |> FsharpFableHelpers.ifBool (fun v -> y <- Domain.Op.Zero)
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
        | Domain.Op.Imagine (p1) -> "Imagine", ConvertDomain.OpCaseImagineToJson (p1)
        | Domain.Op.SumAll (p1) -> "SumAll", JArray (p1 |> Seq.map (fun v -> (v |> ConvertDomain.OpToJson)) |> List.ofSeq)
        | Domain.Op.Zero -> "Zero", JBool (true)
        | _ -> "Unknown", JBool (true)
        |> List.singleton |> Map.ofList |> JObject
    static member OpCaseSumFromJson (json: Json) =
        let mutable p1 = Domain.Op.Unknown
        let mutable p2 = Domain.Op.Unknown
        FsharpFableHelpers.getProps json
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
        FsharpFableHelpers.getProps json
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
        FsharpFableHelpers.getProps json
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
    static member OpCaseQuantumFromJson (json: Json) =
        let mutable p1 = Domain.Op.Unknown
        let mutable p2 = Domain.Op.Unknown
        let mutable p3 = ""
        FsharpFableHelpers.getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "P1" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpFromJson)
            | "P2" -> pair.Value |> (fun v -> p2 <- v |> ConvertDomain.OpFromJson)
            | "P3" -> pair.Value |> FsharpFableHelpers.ifString (fun v -> p3 <- v)
            | _ -> () )
        Domain.Op.Quantum (p1,p2,p3)
    static member OpCaseQuantumToJson (p1,p2,p3) =
        [
            "P1", (p1 |> ConvertDomain.OpToJson)
            "P2", (p2 |> ConvertDomain.OpToJson)
            "P3", JString (p3)
        ] |> Map.ofList |> JObject
    static member OpCaseImagineFromJson (json: Json) =
        let mutable p1 = ValueNone
        FsharpFableHelpers.getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "P1Value" -> pair.Value |> FsharpFableHelpers.ifNumber (fun v -> p1 <- v |> unbox |> ValueSome)
            | _ -> () )
        Domain.Op.Imagine (p1)
    static member OpCaseImagineToJson (p1) =
        [
            match p1 with
            | ValueSome v -> "{firstCharToUpper fieldInfo.Name}Value", JNumber (unbox v)
            | ValueNone -> ()
        ] |> Map.ofList |> JObject
    static member OpErrorFromJson (json: Json): Domain.OpError =
        let mutable y = Domain.OpError.Unknown
        FsharpFableHelpers.getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "General" ->
                let mutable _p1 = ResizeArray()
                pair.Value |> FsharpFableHelpers.ifArray (Seq.iter (FsharpFableHelpers.ifString (fun v -> v |> _p1.Add)))
                y <- _p1 |> List.ofSeq |> Domain.OpError.General
            | "DivisionByZero" -> pair.Value |> FsharpFableHelpers.ifBool (fun v -> y <- Domain.OpError.DivisionByZero)
            | "NotSupported" -> pair.Value |> FsharpFableHelpers.ifBool (fun v -> y <- Domain.OpError.NotSupported)
            | _ -> () )
        y
    static member OpErrorToJson (x:Domain.OpError) =
        match x with
        | Domain.OpError.General (p1) -> "General", JArray (p1 |> Seq.map (fun v -> JString (v)) |> List.ofSeq)
        | Domain.OpError.DivisionByZero -> "DivisionByZero", JBool (true)
        | Domain.OpError.NotSupported -> "NotSupported", JBool (true)
        | _ -> "Unknown", JBool (true)
        |> List.singleton |> Map.ofList |> JObject
    static member OpResultFromJson (json: Json): Domain.OpResult =
        let mutable y = Domain.OpResult.Unknown
        FsharpFableHelpers.getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Success" ->
                let mutable _p1 = 0
                pair.Value |> FsharpFableHelpers.ifNumber (fun v -> _p1 <- v |> unbox)
                y <- _p1 |> Domain.OpResult.Success
            | "Fail" ->
                let mutable _p1 = Domain.OpError.Unknown
                pair.Value |> (fun v -> _p1 <- v |> ConvertDomain.OpErrorFromJson)
                y <- _p1 |> Domain.OpResult.Fail
            | _ -> () )
        y
    static member OpResultToJson (x:Domain.OpResult) =
        match x with
        | Domain.OpResult.Success (p1) -> "Success", JNumber (unbox p1)
        | Domain.OpResult.Fail (p1) -> "Fail", (p1 |> ConvertDomain.OpErrorToJson)
        | _ -> "Unknown", JBool (true)
        |> List.singleton |> Map.ofList |> JObject
    static member RequestFromJson (json: Json): Domain.Request =
        let mutable vToken = ""
        let mutable vOperation = Domain.Op.Unknown
        let mutable vTags = ResizeArray()
        FsharpFableHelpers.getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Token" -> pair.Value |> FsharpFableHelpers.ifString (fun v -> vToken <- v)
            | "Operation" -> pair.Value |> (fun v -> vOperation <- v |> ConvertDomain.OpFromJson)
            | "Tags" -> pair.Value |> FsharpFableHelpers.ifMap (fun (key, value) -> key |> FsharpFableHelpers.ifString (fun v -> v |> fun k -> value |> FsharpFableHelpers.ifString (fun v -> v |> fun v -> vTags.Add(k, v))))
            | _ -> () )
        {
            Token = vToken
            Operation = vOperation
            Tags = vTags |> Map.ofSeq
        }
    static member RequestToJson (x: Domain.Request) =
        [
            "Token", JString (x.Token)
            "Operation", (x.Operation |> ConvertDomain.OpToJson)
            "Tags", x.Tags |> FsharpFableHelpers.mapToArray (fun k -> JString (k)) (fun v -> JString (v))
        ] |> Map.ofList |> JObject
    static member ResponseFromJson (json: Json): Domain.Response =
        let mutable vToken = ""
        let mutable vResult = Domain.OpResult.Unknown
        let mutable vExecutionTime = System.TimeSpan.Zero
        let mutable vExtra = ValueNone
        let mutable vSince = System.DateTime.MinValue
        let mutable vTags = ResizeArray()
        let mutable vStatus = Domain.Subdomain.Status.Unknown
        FsharpFableHelpers.getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Token" -> pair.Value |> FsharpFableHelpers.ifString (fun v -> vToken <- v)
            | "Result" -> pair.Value |> (fun v -> vResult <- v |> ConvertDomain.OpResultFromJson)
            | "ExecutionTime" -> pair.Value |> FsharpFableHelpers.ifString (fun v -> vExecutionTime <- v |> FsharpFableHelpers.toTimeSpan)
            | "ExtraValue" -> pair.Value |> FsharpFableHelpers.ifString (fun v -> vExtra <- v |> ValueSome)
            | "Since" -> pair.Value |> FsharpFableHelpers.ifString (fun v -> vSince <- v |> FsharpFableHelpers.toDateTime)
            | "Tags" -> pair.Value |> FsharpFableHelpers.ifMap (fun (key, value) -> key |> FsharpFableHelpers.ifString (fun v -> v |> fun k -> value |> FsharpFableHelpers.ifString (fun v -> v |> fun v -> vTags.Add(k, v))))
            | "Status" -> pair.Value |> FsharpFableHelpers.ifString (fun v -> vStatus <- v |> ConvertDomainSubdomain.StatusFromString)
            | _ -> () )
        {
            Token = vToken
            Result = vResult
            ExecutionTime = vExecutionTime
            Extra = vExtra
            Since = vSince
            Tags = vTags |> Map.ofSeq
            Status = vStatus
        }
    static member ResponseToJson (x: Domain.Response) =
        [
            "Token", JString (x.Token)
            "Result", (x.Result |> ConvertDomain.OpResultToJson)
            "ExecutionTime", JString (x.ExecutionTime |> FsharpFableHelpers.fromTimeSpan)
            match x.Extra with
            | ValueSome v -> "{firstCharToUpper fieldInfo.Name}Value", JString (v)
            | ValueNone -> ()
            "Since", JString (x.Since |> FsharpFableHelpers.fromDateTime)
            "Tags", x.Tags |> FsharpFableHelpers.mapToArray (fun k -> JString (k)) (fun v -> JString (v))
            "Status", JString (x.Status |> ConvertDomainSubdomain.StatusToString)
        ] |> Map.ofList |> JObject
