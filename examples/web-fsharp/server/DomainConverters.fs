namespace ProtoConverters.FsharpTypes
type ConvertDomain () =
    static member FromProtobuf (x:ProtoClasses.Domain.Op__Val)  =
        Domain.Op.Val
            ((x.P1))
    static member ToProtobufOpCaseVal (p1) : ProtoClasses.Domain.Op__Val =
        let y = ProtoClasses.Domain.Op__Val()
        y.P1 <- p1
        y
    static member FromProtobuf (x:ProtoClasses.Domain.Op__Sum)  =
        Domain.Op.Sum
            ((
                match x.P1Case with
                | ProtoClasses.Domain.Op__Sum.P1OneofCase.P1Val -> x.P1Val |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Sum.P1OneofCase.P1Sum -> x.P1Sum |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Sum.P1OneofCase.P1Mul -> x.P1Mul |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Sum.P1OneofCase.P1Div -> x.P1Div |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Sum.P1OneofCase.P1Ln -> x.P1Ln |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Sum.P1OneofCase.P1Quantum -> x.P1Quantum |> ConvertDomain.FromProtobuf
                | _ -> Domain.Op.Unknown),(
                match x.P2Case with
                | ProtoClasses.Domain.Op__Sum.P2OneofCase.P2Val -> x.P2Val |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Sum.P2OneofCase.P2Sum -> x.P2Sum |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Sum.P2OneofCase.P2Mul -> x.P2Mul |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Sum.P2OneofCase.P2Div -> x.P2Div |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Sum.P2OneofCase.P2Ln -> x.P2Ln |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Sum.P2OneofCase.P2Quantum -> x.P2Quantum |> ConvertDomain.FromProtobuf
                | _ -> Domain.Op.Unknown))
    static member ToProtobufOpCaseSum (p1,p2) : ProtoClasses.Domain.Op__Sum =
        let y = ProtoClasses.Domain.Op__Sum()
        match p1 with
        | Domain.Op.Val (p1) -> y.P1Val <- ConvertDomain.ToProtobufOpCaseVal(p1)
        | Domain.Op.Sum (p1,p2) -> y.P1Sum <- ConvertDomain.ToProtobufOpCaseSum(p1,p2)
        | Domain.Op.Mul (p1,p2) -> y.P1Mul <- ConvertDomain.ToProtobufOpCaseMul(p1,p2)
        | Domain.Op.Div (p1,p2) -> y.P1Div <- ConvertDomain.ToProtobufOpCaseDiv(p1,p2)
        | Domain.Op.Ln (p1) -> y.P1Ln <- ConvertDomain.ToProtobufOpCaseLn(p1)
        | Domain.Op.Quantum (p1,p2,p3) -> y.P1Quantum <- ConvertDomain.ToProtobufOpCaseQuantum(p1,p2,p3)
        | Domain.Op.Unknown -> ()
        match p2 with
        | Domain.Op.Val (p1) -> y.P2Val <- ConvertDomain.ToProtobufOpCaseVal(p1)
        | Domain.Op.Sum (p1,p2) -> y.P2Sum <- ConvertDomain.ToProtobufOpCaseSum(p1,p2)
        | Domain.Op.Mul (p1,p2) -> y.P2Mul <- ConvertDomain.ToProtobufOpCaseMul(p1,p2)
        | Domain.Op.Div (p1,p2) -> y.P2Div <- ConvertDomain.ToProtobufOpCaseDiv(p1,p2)
        | Domain.Op.Ln (p1) -> y.P2Ln <- ConvertDomain.ToProtobufOpCaseLn(p1)
        | Domain.Op.Quantum (p1,p2,p3) -> y.P2Quantum <- ConvertDomain.ToProtobufOpCaseQuantum(p1,p2,p3)
        | Domain.Op.Unknown -> ()
        y
    static member FromProtobuf (x:ProtoClasses.Domain.Op__Mul)  =
        Domain.Op.Mul
            ((
                match x.P1Case with
                | ProtoClasses.Domain.Op__Mul.P1OneofCase.P1Val -> x.P1Val |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Mul.P1OneofCase.P1Sum -> x.P1Sum |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Mul.P1OneofCase.P1Mul -> x.P1Mul |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Mul.P1OneofCase.P1Div -> x.P1Div |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Mul.P1OneofCase.P1Ln -> x.P1Ln |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Mul.P1OneofCase.P1Quantum -> x.P1Quantum |> ConvertDomain.FromProtobuf
                | _ -> Domain.Op.Unknown),(
                match x.P2Case with
                | ProtoClasses.Domain.Op__Mul.P2OneofCase.P2Val -> x.P2Val |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Mul.P2OneofCase.P2Sum -> x.P2Sum |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Mul.P2OneofCase.P2Mul -> x.P2Mul |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Mul.P2OneofCase.P2Div -> x.P2Div |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Mul.P2OneofCase.P2Ln -> x.P2Ln |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Mul.P2OneofCase.P2Quantum -> x.P2Quantum |> ConvertDomain.FromProtobuf
                | _ -> Domain.Op.Unknown))
    static member ToProtobufOpCaseMul (p1,p2) : ProtoClasses.Domain.Op__Mul =
        let y = ProtoClasses.Domain.Op__Mul()
        match p1 with
        | Domain.Op.Val (p1) -> y.P1Val <- ConvertDomain.ToProtobufOpCaseVal(p1)
        | Domain.Op.Sum (p1,p2) -> y.P1Sum <- ConvertDomain.ToProtobufOpCaseSum(p1,p2)
        | Domain.Op.Mul (p1,p2) -> y.P1Mul <- ConvertDomain.ToProtobufOpCaseMul(p1,p2)
        | Domain.Op.Div (p1,p2) -> y.P1Div <- ConvertDomain.ToProtobufOpCaseDiv(p1,p2)
        | Domain.Op.Ln (p1) -> y.P1Ln <- ConvertDomain.ToProtobufOpCaseLn(p1)
        | Domain.Op.Quantum (p1,p2,p3) -> y.P1Quantum <- ConvertDomain.ToProtobufOpCaseQuantum(p1,p2,p3)
        | Domain.Op.Unknown -> ()
        match p2 with
        | Domain.Op.Val (p1) -> y.P2Val <- ConvertDomain.ToProtobufOpCaseVal(p1)
        | Domain.Op.Sum (p1,p2) -> y.P2Sum <- ConvertDomain.ToProtobufOpCaseSum(p1,p2)
        | Domain.Op.Mul (p1,p2) -> y.P2Mul <- ConvertDomain.ToProtobufOpCaseMul(p1,p2)
        | Domain.Op.Div (p1,p2) -> y.P2Div <- ConvertDomain.ToProtobufOpCaseDiv(p1,p2)
        | Domain.Op.Ln (p1) -> y.P2Ln <- ConvertDomain.ToProtobufOpCaseLn(p1)
        | Domain.Op.Quantum (p1,p2,p3) -> y.P2Quantum <- ConvertDomain.ToProtobufOpCaseQuantum(p1,p2,p3)
        | Domain.Op.Unknown -> ()
        y
    static member FromProtobuf (x:ProtoClasses.Domain.Op__Div)  =
        Domain.Op.Div
            ((
                match x.P1Case with
                | ProtoClasses.Domain.Op__Div.P1OneofCase.P1Val -> x.P1Val |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Div.P1OneofCase.P1Sum -> x.P1Sum |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Div.P1OneofCase.P1Mul -> x.P1Mul |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Div.P1OneofCase.P1Div -> x.P1Div |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Div.P1OneofCase.P1Ln -> x.P1Ln |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Div.P1OneofCase.P1Quantum -> x.P1Quantum |> ConvertDomain.FromProtobuf
                | _ -> Domain.Op.Unknown),(
                match x.P2Case with
                | ProtoClasses.Domain.Op__Div.P2OneofCase.P2Val -> x.P2Val |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Div.P2OneofCase.P2Sum -> x.P2Sum |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Div.P2OneofCase.P2Mul -> x.P2Mul |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Div.P2OneofCase.P2Div -> x.P2Div |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Div.P2OneofCase.P2Ln -> x.P2Ln |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Div.P2OneofCase.P2Quantum -> x.P2Quantum |> ConvertDomain.FromProtobuf
                | _ -> Domain.Op.Unknown))
    static member ToProtobufOpCaseDiv (p1,p2) : ProtoClasses.Domain.Op__Div =
        let y = ProtoClasses.Domain.Op__Div()
        match p1 with
        | Domain.Op.Val (p1) -> y.P1Val <- ConvertDomain.ToProtobufOpCaseVal(p1)
        | Domain.Op.Sum (p1,p2) -> y.P1Sum <- ConvertDomain.ToProtobufOpCaseSum(p1,p2)
        | Domain.Op.Mul (p1,p2) -> y.P1Mul <- ConvertDomain.ToProtobufOpCaseMul(p1,p2)
        | Domain.Op.Div (p1,p2) -> y.P1Div <- ConvertDomain.ToProtobufOpCaseDiv(p1,p2)
        | Domain.Op.Ln (p1) -> y.P1Ln <- ConvertDomain.ToProtobufOpCaseLn(p1)
        | Domain.Op.Quantum (p1,p2,p3) -> y.P1Quantum <- ConvertDomain.ToProtobufOpCaseQuantum(p1,p2,p3)
        | Domain.Op.Unknown -> ()
        match p2 with
        | Domain.Op.Val (p1) -> y.P2Val <- ConvertDomain.ToProtobufOpCaseVal(p1)
        | Domain.Op.Sum (p1,p2) -> y.P2Sum <- ConvertDomain.ToProtobufOpCaseSum(p1,p2)
        | Domain.Op.Mul (p1,p2) -> y.P2Mul <- ConvertDomain.ToProtobufOpCaseMul(p1,p2)
        | Domain.Op.Div (p1,p2) -> y.P2Div <- ConvertDomain.ToProtobufOpCaseDiv(p1,p2)
        | Domain.Op.Ln (p1) -> y.P2Ln <- ConvertDomain.ToProtobufOpCaseLn(p1)
        | Domain.Op.Quantum (p1,p2,p3) -> y.P2Quantum <- ConvertDomain.ToProtobufOpCaseQuantum(p1,p2,p3)
        | Domain.Op.Unknown -> ()
        y
    static member FromProtobuf (x:ProtoClasses.Domain.Op__Ln)  =
        Domain.Op.Ln
            ((
                match x.P1Case with
                | ProtoClasses.Domain.Op__Ln.P1OneofCase.P1Val -> x.P1Val |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Ln.P1OneofCase.P1Sum -> x.P1Sum |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Ln.P1OneofCase.P1Mul -> x.P1Mul |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Ln.P1OneofCase.P1Div -> x.P1Div |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Ln.P1OneofCase.P1Ln -> x.P1Ln |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Ln.P1OneofCase.P1Quantum -> x.P1Quantum |> ConvertDomain.FromProtobuf
                | _ -> Domain.Op.Unknown))
    static member ToProtobufOpCaseLn (p1) : ProtoClasses.Domain.Op__Ln =
        let y = ProtoClasses.Domain.Op__Ln()
        match p1 with
        | Domain.Op.Val (p1) -> y.P1Val <- ConvertDomain.ToProtobufOpCaseVal(p1)
        | Domain.Op.Sum (p1,p2) -> y.P1Sum <- ConvertDomain.ToProtobufOpCaseSum(p1,p2)
        | Domain.Op.Mul (p1,p2) -> y.P1Mul <- ConvertDomain.ToProtobufOpCaseMul(p1,p2)
        | Domain.Op.Div (p1,p2) -> y.P1Div <- ConvertDomain.ToProtobufOpCaseDiv(p1,p2)
        | Domain.Op.Ln (p1) -> y.P1Ln <- ConvertDomain.ToProtobufOpCaseLn(p1)
        | Domain.Op.Quantum (p1,p2,p3) -> y.P1Quantum <- ConvertDomain.ToProtobufOpCaseQuantum(p1,p2,p3)
        | Domain.Op.Unknown -> ()
        y
    static member FromProtobuf (x:ProtoClasses.Domain.Op__Quantum)  =
        Domain.Op.Quantum
            ((
                match x.P1Case with
                | ProtoClasses.Domain.Op__Quantum.P1OneofCase.P1Val -> x.P1Val |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Quantum.P1OneofCase.P1Sum -> x.P1Sum |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Quantum.P1OneofCase.P1Mul -> x.P1Mul |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Quantum.P1OneofCase.P1Div -> x.P1Div |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Quantum.P1OneofCase.P1Ln -> x.P1Ln |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Quantum.P1OneofCase.P1Quantum -> x.P1Quantum |> ConvertDomain.FromProtobuf
                | _ -> Domain.Op.Unknown),(
                match x.P2Case with
                | ProtoClasses.Domain.Op__Quantum.P2OneofCase.P2Val -> x.P2Val |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Quantum.P2OneofCase.P2Sum -> x.P2Sum |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Quantum.P2OneofCase.P2Mul -> x.P2Mul |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Quantum.P2OneofCase.P2Div -> x.P2Div |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Quantum.P2OneofCase.P2Ln -> x.P2Ln |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__Quantum.P2OneofCase.P2Quantum -> x.P2Quantum |> ConvertDomain.FromProtobuf
                | _ -> Domain.Op.Unknown),(x.P3))
    static member ToProtobufOpCaseQuantum (p1,p2,p3) : ProtoClasses.Domain.Op__Quantum =
        let y = ProtoClasses.Domain.Op__Quantum()
        match p1 with
        | Domain.Op.Val (p1) -> y.P1Val <- ConvertDomain.ToProtobufOpCaseVal(p1)
        | Domain.Op.Sum (p1,p2) -> y.P1Sum <- ConvertDomain.ToProtobufOpCaseSum(p1,p2)
        | Domain.Op.Mul (p1,p2) -> y.P1Mul <- ConvertDomain.ToProtobufOpCaseMul(p1,p2)
        | Domain.Op.Div (p1,p2) -> y.P1Div <- ConvertDomain.ToProtobufOpCaseDiv(p1,p2)
        | Domain.Op.Ln (p1) -> y.P1Ln <- ConvertDomain.ToProtobufOpCaseLn(p1)
        | Domain.Op.Quantum (p1,p2,p3) -> y.P1Quantum <- ConvertDomain.ToProtobufOpCaseQuantum(p1,p2,p3)
        | Domain.Op.Unknown -> ()
        match p2 with
        | Domain.Op.Val (p1) -> y.P2Val <- ConvertDomain.ToProtobufOpCaseVal(p1)
        | Domain.Op.Sum (p1,p2) -> y.P2Sum <- ConvertDomain.ToProtobufOpCaseSum(p1,p2)
        | Domain.Op.Mul (p1,p2) -> y.P2Mul <- ConvertDomain.ToProtobufOpCaseMul(p1,p2)
        | Domain.Op.Div (p1,p2) -> y.P2Div <- ConvertDomain.ToProtobufOpCaseDiv(p1,p2)
        | Domain.Op.Ln (p1) -> y.P2Ln <- ConvertDomain.ToProtobufOpCaseLn(p1)
        | Domain.Op.Quantum (p1,p2,p3) -> y.P2Quantum <- ConvertDomain.ToProtobufOpCaseQuantum(p1,p2,p3)
        | Domain.Op.Unknown -> ()
        y.P3 <- p3
        y
    static member FromProtobuf (x:ProtoClasses.Domain.OpError__General)  =
        Domain.OpError.General
            ((x.P1))
    static member ToProtobufOpErrorCaseGeneral (p1) : ProtoClasses.Domain.OpError__General =
        let y = ProtoClasses.Domain.OpError__General()
        y.P1 <- p1
        y
    static member FromProtobuf (x:ProtoClasses.Domain.OpResult__Success)  =
        Domain.OpResult.Success
            ((x.P1))
    static member ToProtobufOpResultCaseSuccess (p1) : ProtoClasses.Domain.OpResult__Success =
        let y = ProtoClasses.Domain.OpResult__Success()
        y.P1 <- p1
        y
    static member FromProtobuf (x:ProtoClasses.Domain.OpResult__Fail)  =
        Domain.OpResult.Fail
            ((
                match x.P1Case with
                | ProtoClasses.Domain.OpResult__Fail.P1OneofCase.P1General -> x.P1General |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.OpResult__Fail.P1OneofCase.P1DivisionByZero -> Domain.OpError.DivisionByZero
                | ProtoClasses.Domain.OpResult__Fail.P1OneofCase.P1NotSupported -> Domain.OpError.NotSupported
                | _ -> Domain.OpError.Unknown))
    static member ToProtobufOpResultCaseFail (p1) : ProtoClasses.Domain.OpResult__Fail =
        let y = ProtoClasses.Domain.OpResult__Fail()
        match p1 with
        | Domain.OpError.General (p1) -> y.P1General <- ConvertDomain.ToProtobufOpErrorCaseGeneral(p1)
        | Domain.OpError.DivisionByZero -> y.P1DivisionByZero <- Google.Protobuf.WellKnownTypes.Empty()
        | Domain.OpError.NotSupported -> y.P1NotSupported <- Google.Protobuf.WellKnownTypes.Empty()
        | Domain.OpError.Unknown -> ()
        y
    static member FromProtobuf (x:ProtoClasses.Domain.Request) : Domain.Request =
        {
            Token = x.Token
            Operation = 
                match x.OperationCase with
                | ProtoClasses.Domain.Request.OperationOneofCase.OperationVal -> x.OperationVal |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Request.OperationOneofCase.OperationSum -> x.OperationSum |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Request.OperationOneofCase.OperationMul -> x.OperationMul |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Request.OperationOneofCase.OperationDiv -> x.OperationDiv |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Request.OperationOneofCase.OperationLn -> x.OperationLn |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Request.OperationOneofCase.OperationQuantum -> x.OperationQuantum |> ConvertDomain.FromProtobuf
                | _ -> Domain.Op.Unknown
        }
    static member ToProtobuf (x:Domain.Request) : ProtoClasses.Domain.Request =
        let y = ProtoClasses.Domain.Request()
        y.Token <- x.Token
        match x.Operation with
        | Domain.Op.Val (p1) -> y.OperationVal <- ConvertDomain.ToProtobufOpCaseVal(p1)
        | Domain.Op.Sum (p1,p2) -> y.OperationSum <- ConvertDomain.ToProtobufOpCaseSum(p1,p2)
        | Domain.Op.Mul (p1,p2) -> y.OperationMul <- ConvertDomain.ToProtobufOpCaseMul(p1,p2)
        | Domain.Op.Div (p1,p2) -> y.OperationDiv <- ConvertDomain.ToProtobufOpCaseDiv(p1,p2)
        | Domain.Op.Ln (p1) -> y.OperationLn <- ConvertDomain.ToProtobufOpCaseLn(p1)
        | Domain.Op.Quantum (p1,p2,p3) -> y.OperationQuantum <- ConvertDomain.ToProtobufOpCaseQuantum(p1,p2,p3)
        | Domain.Op.Unknown -> ()
        y
    static member FromProtobuf (x:ProtoClasses.Domain.Response) : Domain.Response =
        {
            Token = x.Token
            Result = 
                match x.ResultCase with
                | ProtoClasses.Domain.Response.ResultOneofCase.ResultSuccess -> x.ResultSuccess |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Response.ResultOneofCase.ResultFail -> x.ResultFail |> ConvertDomain.FromProtobuf
                | _ -> Domain.OpResult.Unknown
            ExecutionTime = x.ExecutionTime |> fun v -> v.ToTimeSpan()
        }
    static member ToProtobuf (x:Domain.Response) : ProtoClasses.Domain.Response =
        let y = ProtoClasses.Domain.Response()
        y.Token <- x.Token
        match x.Result with
        | Domain.OpResult.Success (p1) -> y.ResultSuccess <- ConvertDomain.ToProtobufOpResultCaseSuccess(p1)
        | Domain.OpResult.Fail (p1) -> y.ResultFail <- ConvertDomain.ToProtobufOpResultCaseFail(p1)
        | Domain.OpResult.Unknown -> ()
        y.ExecutionTime <- x.ExecutionTime |> Google.Protobuf.WellKnownTypes.Duration.FromTimeSpan
        y
