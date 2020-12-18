namespace Protogen.FableConverters
open Fable.SimpleJson
open Protogen.FableConverterHelpers
type ConvertDomain () =
    static member OpCaseSumFromJson (json: Json) =
        let mutable p1 = Domain.Op.Unknown
        let mutable p2 = Domain.Op.Unknown
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "P1Val" -> pair.Value |> ifNumber (fun v -> p1 <- v |> unbox |> Domain.Op.Val)
            | "P1Sum" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpCaseSumFromJson)
            | "P1Mul" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpCaseMulFromJson)
            | "P1Div" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpCaseDivFromJson)
            | "P1Ln" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpCaseLnFromJson)
            | "P1Quantum" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpCaseQuantumFromJson)
            | "P1Zero" -> pair.Value |> ifBool (fun v -> p1 <- Domain.Op.Zero)
            | "P2Val" -> pair.Value |> ifNumber (fun v -> p2 <- v |> unbox |> Domain.Op.Val)
            | "P2Sum" -> pair.Value |> (fun v -> p2 <- v |> ConvertDomain.OpCaseSumFromJson)
            | "P2Mul" -> pair.Value |> (fun v -> p2 <- v |> ConvertDomain.OpCaseMulFromJson)
            | "P2Div" -> pair.Value |> (fun v -> p2 <- v |> ConvertDomain.OpCaseDivFromJson)
            | "P2Ln" -> pair.Value |> (fun v -> p2 <- v |> ConvertDomain.OpCaseLnFromJson)
            | "P2Quantum" -> pair.Value |> (fun v -> p2 <- v |> ConvertDomain.OpCaseQuantumFromJson)
            | "P2Zero" -> pair.Value |> ifBool (fun v -> p2 <- Domain.Op.Zero)
            | _ -> () )
        Domain.Op.Sum (p1,p2)
    static member OpCaseSumToJson (p1,p2) =
        [
           match p1 with
           | Domain.Op.Val (p1) -> "P1Val", JNumber (unbox p1)
           | Domain.Op.Sum (p1,p2) -> "P1Sum", ConvertDomain.OpCaseSumToJson (p1,p2)
           | Domain.Op.Mul (p1,p2) -> "P1Mul", ConvertDomain.OpCaseMulToJson (p1,p2)
           | Domain.Op.Div (p1,p2) -> "P1Div", ConvertDomain.OpCaseDivToJson (p1,p2)
           | Domain.Op.Ln (p1) -> "P1Ln", ConvertDomain.OpCaseLnToJson (p1)
           | Domain.Op.Quantum (p1,p2,p3) -> "P1Quantum", ConvertDomain.OpCaseQuantumToJson (p1,p2,p3)
           | Domain.Op.Zero -> "P1Zero", JBool (true)
           | _ -> ()
           match p2 with
           | Domain.Op.Val (p1) -> "P2Val", JNumber (unbox p1)
           | Domain.Op.Sum (p1,p2) -> "P2Sum", ConvertDomain.OpCaseSumToJson (p1,p2)
           | Domain.Op.Mul (p1,p2) -> "P2Mul", ConvertDomain.OpCaseMulToJson (p1,p2)
           | Domain.Op.Div (p1,p2) -> "P2Div", ConvertDomain.OpCaseDivToJson (p1,p2)
           | Domain.Op.Ln (p1) -> "P2Ln", ConvertDomain.OpCaseLnToJson (p1)
           | Domain.Op.Quantum (p1,p2,p3) -> "P2Quantum", ConvertDomain.OpCaseQuantumToJson (p1,p2,p3)
           | Domain.Op.Zero -> "P2Zero", JBool (true)
           | _ -> ()
        ] |> Map.ofList |> JObject
    static member OpCaseMulFromJson (json: Json) =
        let mutable p1 = Domain.Op.Unknown
        let mutable p2 = Domain.Op.Unknown
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "P1Val" -> pair.Value |> ifNumber (fun v -> p1 <- v |> unbox |> Domain.Op.Val)
            | "P1Sum" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpCaseSumFromJson)
            | "P1Mul" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpCaseMulFromJson)
            | "P1Div" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpCaseDivFromJson)
            | "P1Ln" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpCaseLnFromJson)
            | "P1Quantum" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpCaseQuantumFromJson)
            | "P1Zero" -> pair.Value |> ifBool (fun v -> p1 <- Domain.Op.Zero)
            | "P2Val" -> pair.Value |> ifNumber (fun v -> p2 <- v |> unbox |> Domain.Op.Val)
            | "P2Sum" -> pair.Value |> (fun v -> p2 <- v |> ConvertDomain.OpCaseSumFromJson)
            | "P2Mul" -> pair.Value |> (fun v -> p2 <- v |> ConvertDomain.OpCaseMulFromJson)
            | "P2Div" -> pair.Value |> (fun v -> p2 <- v |> ConvertDomain.OpCaseDivFromJson)
            | "P2Ln" -> pair.Value |> (fun v -> p2 <- v |> ConvertDomain.OpCaseLnFromJson)
            | "P2Quantum" -> pair.Value |> (fun v -> p2 <- v |> ConvertDomain.OpCaseQuantumFromJson)
            | "P2Zero" -> pair.Value |> ifBool (fun v -> p2 <- Domain.Op.Zero)
            | _ -> () )
        Domain.Op.Mul (p1,p2)
    static member OpCaseMulToJson (p1,p2) =
        [
           match p1 with
           | Domain.Op.Val (p1) -> "P1Val", JNumber (unbox p1)
           | Domain.Op.Sum (p1,p2) -> "P1Sum", ConvertDomain.OpCaseSumToJson (p1,p2)
           | Domain.Op.Mul (p1,p2) -> "P1Mul", ConvertDomain.OpCaseMulToJson (p1,p2)
           | Domain.Op.Div (p1,p2) -> "P1Div", ConvertDomain.OpCaseDivToJson (p1,p2)
           | Domain.Op.Ln (p1) -> "P1Ln", ConvertDomain.OpCaseLnToJson (p1)
           | Domain.Op.Quantum (p1,p2,p3) -> "P1Quantum", ConvertDomain.OpCaseQuantumToJson (p1,p2,p3)
           | Domain.Op.Zero -> "P1Zero", JBool (true)
           | _ -> ()
           match p2 with
           | Domain.Op.Val (p1) -> "P2Val", JNumber (unbox p1)
           | Domain.Op.Sum (p1,p2) -> "P2Sum", ConvertDomain.OpCaseSumToJson (p1,p2)
           | Domain.Op.Mul (p1,p2) -> "P2Mul", ConvertDomain.OpCaseMulToJson (p1,p2)
           | Domain.Op.Div (p1,p2) -> "P2Div", ConvertDomain.OpCaseDivToJson (p1,p2)
           | Domain.Op.Ln (p1) -> "P2Ln", ConvertDomain.OpCaseLnToJson (p1)
           | Domain.Op.Quantum (p1,p2,p3) -> "P2Quantum", ConvertDomain.OpCaseQuantumToJson (p1,p2,p3)
           | Domain.Op.Zero -> "P2Zero", JBool (true)
           | _ -> ()
        ] |> Map.ofList |> JObject
    static member OpCaseDivFromJson (json: Json) =
        let mutable p1 = Domain.Op.Unknown
        let mutable p2 = Domain.Op.Unknown
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "P1Val" -> pair.Value |> ifNumber (fun v -> p1 <- v |> unbox |> Domain.Op.Val)
            | "P1Sum" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpCaseSumFromJson)
            | "P1Mul" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpCaseMulFromJson)
            | "P1Div" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpCaseDivFromJson)
            | "P1Ln" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpCaseLnFromJson)
            | "P1Quantum" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpCaseQuantumFromJson)
            | "P1Zero" -> pair.Value |> ifBool (fun v -> p1 <- Domain.Op.Zero)
            | "P2Val" -> pair.Value |> ifNumber (fun v -> p2 <- v |> unbox |> Domain.Op.Val)
            | "P2Sum" -> pair.Value |> (fun v -> p2 <- v |> ConvertDomain.OpCaseSumFromJson)
            | "P2Mul" -> pair.Value |> (fun v -> p2 <- v |> ConvertDomain.OpCaseMulFromJson)
            | "P2Div" -> pair.Value |> (fun v -> p2 <- v |> ConvertDomain.OpCaseDivFromJson)
            | "P2Ln" -> pair.Value |> (fun v -> p2 <- v |> ConvertDomain.OpCaseLnFromJson)
            | "P2Quantum" -> pair.Value |> (fun v -> p2 <- v |> ConvertDomain.OpCaseQuantumFromJson)
            | "P2Zero" -> pair.Value |> ifBool (fun v -> p2 <- Domain.Op.Zero)
            | _ -> () )
        Domain.Op.Div (p1,p2)
    static member OpCaseDivToJson (p1,p2) =
        [
           match p1 with
           | Domain.Op.Val (p1) -> "P1Val", JNumber (unbox p1)
           | Domain.Op.Sum (p1,p2) -> "P1Sum", ConvertDomain.OpCaseSumToJson (p1,p2)
           | Domain.Op.Mul (p1,p2) -> "P1Mul", ConvertDomain.OpCaseMulToJson (p1,p2)
           | Domain.Op.Div (p1,p2) -> "P1Div", ConvertDomain.OpCaseDivToJson (p1,p2)
           | Domain.Op.Ln (p1) -> "P1Ln", ConvertDomain.OpCaseLnToJson (p1)
           | Domain.Op.Quantum (p1,p2,p3) -> "P1Quantum", ConvertDomain.OpCaseQuantumToJson (p1,p2,p3)
           | Domain.Op.Zero -> "P1Zero", JBool (true)
           | _ -> ()
           match p2 with
           | Domain.Op.Val (p1) -> "P2Val", JNumber (unbox p1)
           | Domain.Op.Sum (p1,p2) -> "P2Sum", ConvertDomain.OpCaseSumToJson (p1,p2)
           | Domain.Op.Mul (p1,p2) -> "P2Mul", ConvertDomain.OpCaseMulToJson (p1,p2)
           | Domain.Op.Div (p1,p2) -> "P2Div", ConvertDomain.OpCaseDivToJson (p1,p2)
           | Domain.Op.Ln (p1) -> "P2Ln", ConvertDomain.OpCaseLnToJson (p1)
           | Domain.Op.Quantum (p1,p2,p3) -> "P2Quantum", ConvertDomain.OpCaseQuantumToJson (p1,p2,p3)
           | Domain.Op.Zero -> "P2Zero", JBool (true)
           | _ -> ()
        ] |> Map.ofList |> JObject
    static member OpCaseLnFromJson (json: Json) =
        let mutable p1 = Domain.Op.Unknown
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "P1Val" -> pair.Value |> ifNumber (fun v -> p1 <- v |> unbox |> Domain.Op.Val)
            | "P1Sum" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpCaseSumFromJson)
            | "P1Mul" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpCaseMulFromJson)
            | "P1Div" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpCaseDivFromJson)
            | "P1Ln" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpCaseLnFromJson)
            | "P1Quantum" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpCaseQuantumFromJson)
            | "P1Zero" -> pair.Value |> ifBool (fun v -> p1 <- Domain.Op.Zero)
            | _ -> () )
        Domain.Op.Ln (p1)
    static member OpCaseLnToJson (p1) =
        [
           match p1 with
           | Domain.Op.Val (p1) -> "P1Val", JNumber (unbox p1)
           | Domain.Op.Sum (p1,p2) -> "P1Sum", ConvertDomain.OpCaseSumToJson (p1,p2)
           | Domain.Op.Mul (p1,p2) -> "P1Mul", ConvertDomain.OpCaseMulToJson (p1,p2)
           | Domain.Op.Div (p1,p2) -> "P1Div", ConvertDomain.OpCaseDivToJson (p1,p2)
           | Domain.Op.Ln (p1) -> "P1Ln", ConvertDomain.OpCaseLnToJson (p1)
           | Domain.Op.Quantum (p1,p2,p3) -> "P1Quantum", ConvertDomain.OpCaseQuantumToJson (p1,p2,p3)
           | Domain.Op.Zero -> "P1Zero", JBool (true)
           | _ -> ()
        ] |> Map.ofList |> JObject
    static member OpCaseQuantumFromJson (json: Json) =
        let mutable p1 = Domain.Op.Unknown
        let mutable p2 = Domain.Op.Unknown
        let mutable p3 = ""
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "P1Val" -> pair.Value |> ifNumber (fun v -> p1 <- v |> unbox |> Domain.Op.Val)
            | "P1Sum" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpCaseSumFromJson)
            | "P1Mul" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpCaseMulFromJson)
            | "P1Div" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpCaseDivFromJson)
            | "P1Ln" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpCaseLnFromJson)
            | "P1Quantum" -> pair.Value |> (fun v -> p1 <- v |> ConvertDomain.OpCaseQuantumFromJson)
            | "P1Zero" -> pair.Value |> ifBool (fun v -> p1 <- Domain.Op.Zero)
            | "P2Val" -> pair.Value |> ifNumber (fun v -> p2 <- v |> unbox |> Domain.Op.Val)
            | "P2Sum" -> pair.Value |> (fun v -> p2 <- v |> ConvertDomain.OpCaseSumFromJson)
            | "P2Mul" -> pair.Value |> (fun v -> p2 <- v |> ConvertDomain.OpCaseMulFromJson)
            | "P2Div" -> pair.Value |> (fun v -> p2 <- v |> ConvertDomain.OpCaseDivFromJson)
            | "P2Ln" -> pair.Value |> (fun v -> p2 <- v |> ConvertDomain.OpCaseLnFromJson)
            | "P2Quantum" -> pair.Value |> (fun v -> p2 <- v |> ConvertDomain.OpCaseQuantumFromJson)
            | "P2Zero" -> pair.Value |> ifBool (fun v -> p2 <- Domain.Op.Zero)
            | "P3" -> pair.Value |> ifString (fun v -> p3 <- v)
            | _ -> () )
        Domain.Op.Quantum (p1,p2,p3)
    static member OpCaseQuantumToJson (p1,p2,p3) =
        [
           match p1 with
           | Domain.Op.Val (p1) -> "P1Val", JNumber (unbox p1)
           | Domain.Op.Sum (p1,p2) -> "P1Sum", ConvertDomain.OpCaseSumToJson (p1,p2)
           | Domain.Op.Mul (p1,p2) -> "P1Mul", ConvertDomain.OpCaseMulToJson (p1,p2)
           | Domain.Op.Div (p1,p2) -> "P1Div", ConvertDomain.OpCaseDivToJson (p1,p2)
           | Domain.Op.Ln (p1) -> "P1Ln", ConvertDomain.OpCaseLnToJson (p1)
           | Domain.Op.Quantum (p1,p2,p3) -> "P1Quantum", ConvertDomain.OpCaseQuantumToJson (p1,p2,p3)
           | Domain.Op.Zero -> "P1Zero", JBool (true)
           | _ -> ()
           match p2 with
           | Domain.Op.Val (p1) -> "P2Val", JNumber (unbox p1)
           | Domain.Op.Sum (p1,p2) -> "P2Sum", ConvertDomain.OpCaseSumToJson (p1,p2)
           | Domain.Op.Mul (p1,p2) -> "P2Mul", ConvertDomain.OpCaseMulToJson (p1,p2)
           | Domain.Op.Div (p1,p2) -> "P2Div", ConvertDomain.OpCaseDivToJson (p1,p2)
           | Domain.Op.Ln (p1) -> "P2Ln", ConvertDomain.OpCaseLnToJson (p1)
           | Domain.Op.Quantum (p1,p2,p3) -> "P2Quantum", ConvertDomain.OpCaseQuantumToJson (p1,p2,p3)
           | Domain.Op.Zero -> "P2Zero", JBool (true)
           | _ -> ()
           "P3", JString (p3)
        ] |> Map.ofList |> JObject
    static member OpResultCaseFailFromJson (json: Json) =
        let mutable p1 = Domain.OpError.Unknown
        getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "P1General" -> pair.Value |> ifString (fun v -> p1 <- v |> Domain.OpError.General)
            | "P1DivisionByZero" -> pair.Value |> ifBool (fun v -> p1 <- Domain.OpError.DivisionByZero)
            | "P1NotSupported" -> pair.Value |> ifBool (fun v -> p1 <- Domain.OpError.NotSupported)
            | _ -> () )
        Domain.OpResult.Fail (p1)
    static member OpResultCaseFailToJson (p1) =
        [
           match p1 with
           | Domain.OpError.General (p1) -> "P1General", JString (p1)
           | Domain.OpError.DivisionByZero -> "P1DivisionByZero", JBool (true)
           | Domain.OpError.NotSupported -> "P1NotSupported", JBool (true)
           | _ -> ()
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
            | "OperationVal" -> pair.Value |> ifNumber (fun v -> vOperation <- v |> unbox |> Domain.Op.Val)
            | "OperationSum" -> pair.Value |> (fun v -> vOperation <- v |> ConvertDomain.OpCaseSumFromJson)
            | "OperationMul" -> pair.Value |> (fun v -> vOperation <- v |> ConvertDomain.OpCaseMulFromJson)
            | "OperationDiv" -> pair.Value |> (fun v -> vOperation <- v |> ConvertDomain.OpCaseDivFromJson)
            | "OperationLn" -> pair.Value |> (fun v -> vOperation <- v |> ConvertDomain.OpCaseLnFromJson)
            | "OperationQuantum" -> pair.Value |> (fun v -> vOperation <- v |> ConvertDomain.OpCaseQuantumFromJson)
            | "OperationZero" -> pair.Value |> ifBool (fun v -> vOperation <- Domain.Op.Zero)
            | _ -> () )
        {
            Token = vToken
            Operation = vOperation
        }
    static member RequestToJson (x: Domain.Request) =
        [
           "Token", JString (x.Token)
           match x.Operation with
           | Domain.Op.Val (p1) -> "OperationVal", JNumber (unbox p1)
           | Domain.Op.Sum (p1,p2) -> "OperationSum", ConvertDomain.OpCaseSumToJson (p1,p2)
           | Domain.Op.Mul (p1,p2) -> "OperationMul", ConvertDomain.OpCaseMulToJson (p1,p2)
           | Domain.Op.Div (p1,p2) -> "OperationDiv", ConvertDomain.OpCaseDivToJson (p1,p2)
           | Domain.Op.Ln (p1) -> "OperationLn", ConvertDomain.OpCaseLnToJson (p1)
           | Domain.Op.Quantum (p1,p2,p3) -> "OperationQuantum", ConvertDomain.OpCaseQuantumToJson (p1,p2,p3)
           | Domain.Op.Zero -> "OperationZero", JBool (true)
           | _ -> ()
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
            | "ResultSuccess" -> pair.Value |> ifNumber (fun v -> vResult <- v |> unbox |> Domain.OpResult.Success)
            | "ResultFail" -> pair.Value |> (fun v -> vResult <- v |> ConvertDomain.OpResultCaseFailFromJson)
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
           match x.Result with
           | Domain.OpResult.Success (p1) -> "ResultSuccess", JNumber (unbox p1)
           | Domain.OpResult.Fail (p1) -> "ResultFail", ConvertDomain.OpResultCaseFailToJson (p1)
           | _ -> ()
           "ExecutionTime", JString (x.ExecutionTime |> fromTimeSpan)
           match x.Extra with
           | Some v -> "ExtraValue", JString (v)
           | None -> ()
           "Since", JString (x.Since |> fromDateTimeOffset)
           "Tags", JObject (x.Tags |> Map.map (fun _ v -> JString (v)))
        ] |> Map.ofList |> JObject
