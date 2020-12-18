namespace Protogen.FsharpConverters
type ConvertDomain () =
    static member FromProtobuf (x:ProtoClasses.Domain.Op__ComplexName ["Sum"; "Op"; "Domain"])  =
        Domain.Op.Sum
            ((
                match x.P1Case with
                | ProtoClasses.Domain.Op__ComplexName ["Sum"; "Op"; "Domain"].P1OneofCase.P1Val -> Domain.Op.Val(x.P1Val)
                | ProtoClasses.Domain.Op__ComplexName ["Sum"; "Op"; "Domain"].P1OneofCase.P1Sum -> x.P1Sum |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Sum"; "Op"; "Domain"].P1OneofCase.P1Mul -> x.P1Mul |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Sum"; "Op"; "Domain"].P1OneofCase.P1Div -> x.P1Div |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Sum"; "Op"; "Domain"].P1OneofCase.P1Ln -> x.P1Ln |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Sum"; "Op"; "Domain"].P1OneofCase.P1Quantum -> x.P1Quantum |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Sum"; "Op"; "Domain"].P1OneofCase.P1Zero -> Domain.Op.Zero
                | _ -> Domain.Op.Unknown),(
                match x.P2Case with
                | ProtoClasses.Domain.Op__ComplexName ["Sum"; "Op"; "Domain"].P2OneofCase.P2Val -> Domain.Op.Val(x.P2Val)
                | ProtoClasses.Domain.Op__ComplexName ["Sum"; "Op"; "Domain"].P2OneofCase.P2Sum -> x.P2Sum |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Sum"; "Op"; "Domain"].P2OneofCase.P2Mul -> x.P2Mul |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Sum"; "Op"; "Domain"].P2OneofCase.P2Div -> x.P2Div |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Sum"; "Op"; "Domain"].P2OneofCase.P2Ln -> x.P2Ln |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Sum"; "Op"; "Domain"].P2OneofCase.P2Quantum -> x.P2Quantum |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Sum"; "Op"; "Domain"].P2OneofCase.P2Zero -> Domain.Op.Zero
                | _ -> Domain.Op.Unknown))
    static member ComplexName ["Op"; "Domain"]CaseComplexName ["Sum"; "Op"; "Domain"]ToProtobuf (p1,p2) : ProtoClasses.Domain.Op__ComplexName ["Sum"; "Op"; "Domain"] =
        let y = ProtoClasses.Domain.Op__ComplexName ["Sum"; "Op"; "Domain"]()
        match p1 with
        | Domain.Op.Val (p1) ->
            y.P1Val <- p1
        | Domain.Op.Sum (p1,p2) -> y.P1Sum <- ConvertDomain.OpCaseSumToProtobuf(p1,p2)
        | Domain.Op.Mul (p1,p2) -> y.P1Mul <- ConvertDomain.OpCaseMulToProtobuf(p1,p2)
        | Domain.Op.Div (p1,p2) -> y.P1Div <- ConvertDomain.OpCaseDivToProtobuf(p1,p2)
        | Domain.Op.Ln (p1) -> y.P1Ln <- ConvertDomain.OpCaseLnToProtobuf(p1)
        | Domain.Op.Quantum (p1,p2,p3) -> y.P1Quantum <- ConvertDomain.OpCaseQuantumToProtobuf(p1,p2,p3)
        | Domain.Op.Zero -> y.P1Zero <- true
        | Domain.Op.Unknown -> ()
        match p2 with
        | Domain.Op.Val (p1) ->
            y.P2Val <- p1
        | Domain.Op.Sum (p1,p2) -> y.P2Sum <- ConvertDomain.OpCaseSumToProtobuf(p1,p2)
        | Domain.Op.Mul (p1,p2) -> y.P2Mul <- ConvertDomain.OpCaseMulToProtobuf(p1,p2)
        | Domain.Op.Div (p1,p2) -> y.P2Div <- ConvertDomain.OpCaseDivToProtobuf(p1,p2)
        | Domain.Op.Ln (p1) -> y.P2Ln <- ConvertDomain.OpCaseLnToProtobuf(p1)
        | Domain.Op.Quantum (p1,p2,p3) -> y.P2Quantum <- ConvertDomain.OpCaseQuantumToProtobuf(p1,p2,p3)
        | Domain.Op.Zero -> y.P2Zero <- true
        | Domain.Op.Unknown -> ()
        y
    static member FromProtobuf (x:ProtoClasses.Domain.Op__ComplexName ["Mul"; "Op"; "Domain"])  =
        Domain.Op.Mul
            ((
                match x.P1Case with
                | ProtoClasses.Domain.Op__ComplexName ["Mul"; "Op"; "Domain"].P1OneofCase.P1Val -> Domain.Op.Val(x.P1Val)
                | ProtoClasses.Domain.Op__ComplexName ["Mul"; "Op"; "Domain"].P1OneofCase.P1Sum -> x.P1Sum |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Mul"; "Op"; "Domain"].P1OneofCase.P1Mul -> x.P1Mul |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Mul"; "Op"; "Domain"].P1OneofCase.P1Div -> x.P1Div |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Mul"; "Op"; "Domain"].P1OneofCase.P1Ln -> x.P1Ln |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Mul"; "Op"; "Domain"].P1OneofCase.P1Quantum -> x.P1Quantum |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Mul"; "Op"; "Domain"].P1OneofCase.P1Zero -> Domain.Op.Zero
                | _ -> Domain.Op.Unknown),(
                match x.P2Case with
                | ProtoClasses.Domain.Op__ComplexName ["Mul"; "Op"; "Domain"].P2OneofCase.P2Val -> Domain.Op.Val(x.P2Val)
                | ProtoClasses.Domain.Op__ComplexName ["Mul"; "Op"; "Domain"].P2OneofCase.P2Sum -> x.P2Sum |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Mul"; "Op"; "Domain"].P2OneofCase.P2Mul -> x.P2Mul |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Mul"; "Op"; "Domain"].P2OneofCase.P2Div -> x.P2Div |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Mul"; "Op"; "Domain"].P2OneofCase.P2Ln -> x.P2Ln |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Mul"; "Op"; "Domain"].P2OneofCase.P2Quantum -> x.P2Quantum |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Mul"; "Op"; "Domain"].P2OneofCase.P2Zero -> Domain.Op.Zero
                | _ -> Domain.Op.Unknown))
    static member ComplexName ["Op"; "Domain"]CaseComplexName ["Mul"; "Op"; "Domain"]ToProtobuf (p1,p2) : ProtoClasses.Domain.Op__ComplexName ["Mul"; "Op"; "Domain"] =
        let y = ProtoClasses.Domain.Op__ComplexName ["Mul"; "Op"; "Domain"]()
        match p1 with
        | Domain.Op.Val (p1) ->
            y.P1Val <- p1
        | Domain.Op.Sum (p1,p2) -> y.P1Sum <- ConvertDomain.OpCaseSumToProtobuf(p1,p2)
        | Domain.Op.Mul (p1,p2) -> y.P1Mul <- ConvertDomain.OpCaseMulToProtobuf(p1,p2)
        | Domain.Op.Div (p1,p2) -> y.P1Div <- ConvertDomain.OpCaseDivToProtobuf(p1,p2)
        | Domain.Op.Ln (p1) -> y.P1Ln <- ConvertDomain.OpCaseLnToProtobuf(p1)
        | Domain.Op.Quantum (p1,p2,p3) -> y.P1Quantum <- ConvertDomain.OpCaseQuantumToProtobuf(p1,p2,p3)
        | Domain.Op.Zero -> y.P1Zero <- true
        | Domain.Op.Unknown -> ()
        match p2 with
        | Domain.Op.Val (p1) ->
            y.P2Val <- p1
        | Domain.Op.Sum (p1,p2) -> y.P2Sum <- ConvertDomain.OpCaseSumToProtobuf(p1,p2)
        | Domain.Op.Mul (p1,p2) -> y.P2Mul <- ConvertDomain.OpCaseMulToProtobuf(p1,p2)
        | Domain.Op.Div (p1,p2) -> y.P2Div <- ConvertDomain.OpCaseDivToProtobuf(p1,p2)
        | Domain.Op.Ln (p1) -> y.P2Ln <- ConvertDomain.OpCaseLnToProtobuf(p1)
        | Domain.Op.Quantum (p1,p2,p3) -> y.P2Quantum <- ConvertDomain.OpCaseQuantumToProtobuf(p1,p2,p3)
        | Domain.Op.Zero -> y.P2Zero <- true
        | Domain.Op.Unknown -> ()
        y
    static member FromProtobuf (x:ProtoClasses.Domain.Op__ComplexName ["Div"; "Op"; "Domain"])  =
        Domain.Op.Div
            ((
                match x.P1Case with
                | ProtoClasses.Domain.Op__ComplexName ["Div"; "Op"; "Domain"].P1OneofCase.P1Val -> Domain.Op.Val(x.P1Val)
                | ProtoClasses.Domain.Op__ComplexName ["Div"; "Op"; "Domain"].P1OneofCase.P1Sum -> x.P1Sum |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Div"; "Op"; "Domain"].P1OneofCase.P1Mul -> x.P1Mul |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Div"; "Op"; "Domain"].P1OneofCase.P1Div -> x.P1Div |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Div"; "Op"; "Domain"].P1OneofCase.P1Ln -> x.P1Ln |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Div"; "Op"; "Domain"].P1OneofCase.P1Quantum -> x.P1Quantum |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Div"; "Op"; "Domain"].P1OneofCase.P1Zero -> Domain.Op.Zero
                | _ -> Domain.Op.Unknown),(
                match x.P2Case with
                | ProtoClasses.Domain.Op__ComplexName ["Div"; "Op"; "Domain"].P2OneofCase.P2Val -> Domain.Op.Val(x.P2Val)
                | ProtoClasses.Domain.Op__ComplexName ["Div"; "Op"; "Domain"].P2OneofCase.P2Sum -> x.P2Sum |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Div"; "Op"; "Domain"].P2OneofCase.P2Mul -> x.P2Mul |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Div"; "Op"; "Domain"].P2OneofCase.P2Div -> x.P2Div |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Div"; "Op"; "Domain"].P2OneofCase.P2Ln -> x.P2Ln |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Div"; "Op"; "Domain"].P2OneofCase.P2Quantum -> x.P2Quantum |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Div"; "Op"; "Domain"].P2OneofCase.P2Zero -> Domain.Op.Zero
                | _ -> Domain.Op.Unknown))
    static member ComplexName ["Op"; "Domain"]CaseComplexName ["Div"; "Op"; "Domain"]ToProtobuf (p1,p2) : ProtoClasses.Domain.Op__ComplexName ["Div"; "Op"; "Domain"] =
        let y = ProtoClasses.Domain.Op__ComplexName ["Div"; "Op"; "Domain"]()
        match p1 with
        | Domain.Op.Val (p1) ->
            y.P1Val <- p1
        | Domain.Op.Sum (p1,p2) -> y.P1Sum <- ConvertDomain.OpCaseSumToProtobuf(p1,p2)
        | Domain.Op.Mul (p1,p2) -> y.P1Mul <- ConvertDomain.OpCaseMulToProtobuf(p1,p2)
        | Domain.Op.Div (p1,p2) -> y.P1Div <- ConvertDomain.OpCaseDivToProtobuf(p1,p2)
        | Domain.Op.Ln (p1) -> y.P1Ln <- ConvertDomain.OpCaseLnToProtobuf(p1)
        | Domain.Op.Quantum (p1,p2,p3) -> y.P1Quantum <- ConvertDomain.OpCaseQuantumToProtobuf(p1,p2,p3)
        | Domain.Op.Zero -> y.P1Zero <- true
        | Domain.Op.Unknown -> ()
        match p2 with
        | Domain.Op.Val (p1) ->
            y.P2Val <- p1
        | Domain.Op.Sum (p1,p2) -> y.P2Sum <- ConvertDomain.OpCaseSumToProtobuf(p1,p2)
        | Domain.Op.Mul (p1,p2) -> y.P2Mul <- ConvertDomain.OpCaseMulToProtobuf(p1,p2)
        | Domain.Op.Div (p1,p2) -> y.P2Div <- ConvertDomain.OpCaseDivToProtobuf(p1,p2)
        | Domain.Op.Ln (p1) -> y.P2Ln <- ConvertDomain.OpCaseLnToProtobuf(p1)
        | Domain.Op.Quantum (p1,p2,p3) -> y.P2Quantum <- ConvertDomain.OpCaseQuantumToProtobuf(p1,p2,p3)
        | Domain.Op.Zero -> y.P2Zero <- true
        | Domain.Op.Unknown -> ()
        y
    static member FromProtobuf (x:ProtoClasses.Domain.Op__ComplexName ["Ln"; "Op"; "Domain"])  =
        Domain.Op.Ln
            ((
                match x.P1Case with
                | ProtoClasses.Domain.Op__ComplexName ["Ln"; "Op"; "Domain"].P1OneofCase.P1Val -> Domain.Op.Val(x.P1Val)
                | ProtoClasses.Domain.Op__ComplexName ["Ln"; "Op"; "Domain"].P1OneofCase.P1Sum -> x.P1Sum |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Ln"; "Op"; "Domain"].P1OneofCase.P1Mul -> x.P1Mul |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Ln"; "Op"; "Domain"].P1OneofCase.P1Div -> x.P1Div |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Ln"; "Op"; "Domain"].P1OneofCase.P1Ln -> x.P1Ln |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Ln"; "Op"; "Domain"].P1OneofCase.P1Quantum -> x.P1Quantum |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Ln"; "Op"; "Domain"].P1OneofCase.P1Zero -> Domain.Op.Zero
                | _ -> Domain.Op.Unknown))
    static member ComplexName ["Op"; "Domain"]CaseComplexName ["Ln"; "Op"; "Domain"]ToProtobuf (p1) : ProtoClasses.Domain.Op__ComplexName ["Ln"; "Op"; "Domain"] =
        let y = ProtoClasses.Domain.Op__ComplexName ["Ln"; "Op"; "Domain"]()
        match p1 with
        | Domain.Op.Val (p1) ->
            y.P1Val <- p1
        | Domain.Op.Sum (p1,p2) -> y.P1Sum <- ConvertDomain.OpCaseSumToProtobuf(p1,p2)
        | Domain.Op.Mul (p1,p2) -> y.P1Mul <- ConvertDomain.OpCaseMulToProtobuf(p1,p2)
        | Domain.Op.Div (p1,p2) -> y.P1Div <- ConvertDomain.OpCaseDivToProtobuf(p1,p2)
        | Domain.Op.Ln (p1) -> y.P1Ln <- ConvertDomain.OpCaseLnToProtobuf(p1)
        | Domain.Op.Quantum (p1,p2,p3) -> y.P1Quantum <- ConvertDomain.OpCaseQuantumToProtobuf(p1,p2,p3)
        | Domain.Op.Zero -> y.P1Zero <- true
        | Domain.Op.Unknown -> ()
        y
    static member FromProtobuf (x:ProtoClasses.Domain.Op__ComplexName ["Quantum"; "Op"; "Domain"])  =
        Domain.Op.Quantum
            ((
                match x.P1Case with
                | ProtoClasses.Domain.Op__ComplexName ["Quantum"; "Op"; "Domain"].P1OneofCase.P1Val -> Domain.Op.Val(x.P1Val)
                | ProtoClasses.Domain.Op__ComplexName ["Quantum"; "Op"; "Domain"].P1OneofCase.P1Sum -> x.P1Sum |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Quantum"; "Op"; "Domain"].P1OneofCase.P1Mul -> x.P1Mul |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Quantum"; "Op"; "Domain"].P1OneofCase.P1Div -> x.P1Div |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Quantum"; "Op"; "Domain"].P1OneofCase.P1Ln -> x.P1Ln |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Quantum"; "Op"; "Domain"].P1OneofCase.P1Quantum -> x.P1Quantum |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Quantum"; "Op"; "Domain"].P1OneofCase.P1Zero -> Domain.Op.Zero
                | _ -> Domain.Op.Unknown),(
                match x.P2Case with
                | ProtoClasses.Domain.Op__ComplexName ["Quantum"; "Op"; "Domain"].P2OneofCase.P2Val -> Domain.Op.Val(x.P2Val)
                | ProtoClasses.Domain.Op__ComplexName ["Quantum"; "Op"; "Domain"].P2OneofCase.P2Sum -> x.P2Sum |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Quantum"; "Op"; "Domain"].P2OneofCase.P2Mul -> x.P2Mul |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Quantum"; "Op"; "Domain"].P2OneofCase.P2Div -> x.P2Div |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Quantum"; "Op"; "Domain"].P2OneofCase.P2Ln -> x.P2Ln |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Quantum"; "Op"; "Domain"].P2OneofCase.P2Quantum -> x.P2Quantum |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Op__ComplexName ["Quantum"; "Op"; "Domain"].P2OneofCase.P2Zero -> Domain.Op.Zero
                | _ -> Domain.Op.Unknown),(x.P3))
    static member ComplexName ["Op"; "Domain"]CaseComplexName ["Quantum"; "Op"; "Domain"]ToProtobuf (p1,p2,p3) : ProtoClasses.Domain.Op__ComplexName ["Quantum"; "Op"; "Domain"] =
        let y = ProtoClasses.Domain.Op__ComplexName ["Quantum"; "Op"; "Domain"]()
        match p1 with
        | Domain.Op.Val (p1) ->
            y.P1Val <- p1
        | Domain.Op.Sum (p1,p2) -> y.P1Sum <- ConvertDomain.OpCaseSumToProtobuf(p1,p2)
        | Domain.Op.Mul (p1,p2) -> y.P1Mul <- ConvertDomain.OpCaseMulToProtobuf(p1,p2)
        | Domain.Op.Div (p1,p2) -> y.P1Div <- ConvertDomain.OpCaseDivToProtobuf(p1,p2)
        | Domain.Op.Ln (p1) -> y.P1Ln <- ConvertDomain.OpCaseLnToProtobuf(p1)
        | Domain.Op.Quantum (p1,p2,p3) -> y.P1Quantum <- ConvertDomain.OpCaseQuantumToProtobuf(p1,p2,p3)
        | Domain.Op.Zero -> y.P1Zero <- true
        | Domain.Op.Unknown -> ()
        match p2 with
        | Domain.Op.Val (p1) ->
            y.P2Val <- p1
        | Domain.Op.Sum (p1,p2) -> y.P2Sum <- ConvertDomain.OpCaseSumToProtobuf(p1,p2)
        | Domain.Op.Mul (p1,p2) -> y.P2Mul <- ConvertDomain.OpCaseMulToProtobuf(p1,p2)
        | Domain.Op.Div (p1,p2) -> y.P2Div <- ConvertDomain.OpCaseDivToProtobuf(p1,p2)
        | Domain.Op.Ln (p1) -> y.P2Ln <- ConvertDomain.OpCaseLnToProtobuf(p1)
        | Domain.Op.Quantum (p1,p2,p3) -> y.P2Quantum <- ConvertDomain.OpCaseQuantumToProtobuf(p1,p2,p3)
        | Domain.Op.Zero -> y.P2Zero <- true
        | Domain.Op.Unknown -> ()
        y.P3 <- p3
        y
    static member FromProtobuf (x:ProtoClasses.Domain.OpResult__ComplexName ["Fail"; "OpResult"; "Domain"])  =
        Domain.OpResult.Fail
            ((
                match x.P1Case with
                | ProtoClasses.Domain.OpResult__ComplexName ["Fail"; "OpResult"; "Domain"].P1OneofCase.P1General -> Domain.OpError.General(x.P1General)
                | ProtoClasses.Domain.OpResult__ComplexName ["Fail"; "OpResult"; "Domain"].P1OneofCase.P1DivisionByZero -> Domain.OpError.DivisionByZero
                | ProtoClasses.Domain.OpResult__ComplexName ["Fail"; "OpResult"; "Domain"].P1OneofCase.P1NotSupported -> Domain.OpError.NotSupported
                | _ -> Domain.OpError.Unknown))
    static member ComplexName ["OpResult"; "Domain"]CaseComplexName ["Fail"; "OpResult"; "Domain"]ToProtobuf (p1) : ProtoClasses.Domain.OpResult__ComplexName ["Fail"; "OpResult"; "Domain"] =
        let y = ProtoClasses.Domain.OpResult__ComplexName ["Fail"; "OpResult"; "Domain"]()
        match p1 with
        | Domain.OpError.General (p1) ->
            y.P1General <- p1
        | Domain.OpError.DivisionByZero -> y.P1DivisionByZero <- true
        | Domain.OpError.NotSupported -> y.P1NotSupported <- true
        | Domain.OpError.Unknown -> ()
        y
    static member FromProtobuf (x:ProtoClasses.Domain.Request) : Domain.Request =
        {
            Token = x.Token
            Operation = 
                match x.OperationCase with
                | ProtoClasses.Domain.Request.OperationOneofCase.OperationVal -> Domain.Op.Val(x.OperationVal)
                | ProtoClasses.Domain.Request.OperationOneofCase.OperationSum -> x.OperationSum |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Request.OperationOneofCase.OperationMul -> x.OperationMul |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Request.OperationOneofCase.OperationDiv -> x.OperationDiv |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Request.OperationOneofCase.OperationLn -> x.OperationLn |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Request.OperationOneofCase.OperationQuantum -> x.OperationQuantum |> ConvertDomain.FromProtobuf
                | ProtoClasses.Domain.Request.OperationOneofCase.OperationZero -> Domain.Op.Zero
                | _ -> Domain.Op.Unknown
        }
    static member ToProtobuf (x:Domain.Request) : ProtoClasses.Domain.Request =
        let y = ProtoClasses.Domain.Request()
        y.Token <- x.Token
        match x.Operation with
        | Domain.Op.Val (p1) ->
            y.OperationVal <- p1
        | Domain.Op.Sum (p1,p2) -> y.OperationSum <- ConvertDomain.OpCaseSumToProtobuf(p1,p2)
        | Domain.Op.Mul (p1,p2) -> y.OperationMul <- ConvertDomain.OpCaseMulToProtobuf(p1,p2)
        | Domain.Op.Div (p1,p2) -> y.OperationDiv <- ConvertDomain.OpCaseDivToProtobuf(p1,p2)
        | Domain.Op.Ln (p1) -> y.OperationLn <- ConvertDomain.OpCaseLnToProtobuf(p1)
        | Domain.Op.Quantum (p1,p2,p3) -> y.OperationQuantum <- ConvertDomain.OpCaseQuantumToProtobuf(p1,p2,p3)
        | Domain.Op.Zero -> y.OperationZero <- true
        | Domain.Op.Unknown -> ()
        y
    static member FromProtobuf (x:ProtoClasses.Domain.Response) : Domain.Response =
        {
            Token = x.Token
            Result = 
                match x.ResultCase with
                | ProtoClasses.Domain.Response.ResultOneofCase.ResultSuccess -> Domain.OpResult.Success(x.ResultSuccess)
                | ProtoClasses.Domain.Response.ResultOneofCase.ResultFail -> x.ResultFail |> ConvertDomain.FromProtobuf
                | _ -> Domain.OpResult.Unknown
            ExecutionTime = x.ExecutionTime |> fun v -> v.ToTimeSpan()
            Extra = if x.ExtraCase = ProtoClasses.Domain.Response.ExtraOneofCase.ExtraValue then Some (x.ExtraValue) else None
            Since = x.Since |> fun v -> v.ToDateTimeOffset()
            Tags = x.Tags |> Seq.map(fun pair -> pair.Key,pair.Value) |> Map.ofSeq
        }
    static member ToProtobuf (x:Domain.Response) : ProtoClasses.Domain.Response =
        let y = ProtoClasses.Domain.Response()
        y.Token <- x.Token
        match x.Result with
        | Domain.OpResult.Success (p1) ->
            y.ResultSuccess <- p1
        | Domain.OpResult.Fail (p1) -> y.ResultFail <- ConvertDomain.OpResultCaseFailToProtobuf(p1)
        | Domain.OpResult.Unknown -> ()
        y.ExecutionTime <- x.ExecutionTime |> Google.Protobuf.WellKnownTypes.Duration.FromTimeSpan
        match x.Extra with
        | Some v -> y.ExtraValue <- v
        | None -> ()
        y.Since <- x.Since |> Google.Protobuf.WellKnownTypes.Timestamp.FromDateTimeOffset
        y.Tags.Add(x.Tags)
        y
