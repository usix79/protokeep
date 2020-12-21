namespace Protogen.FsharpConverters
type ConvertDomain () =
    static member FromProtobuf (x:ProtoClasses.Domain.Op) : Domain.Op =
        match x.UnionCase with
        | ProtoClasses.Domain.Op.UnionOneofCase.Val -> Domain.Op.Val(x.Val)
        | ProtoClasses.Domain.Op.UnionOneofCase.Sum -> x.Sum |> ConvertDomain.FromProtobuf
        | ProtoClasses.Domain.Op.UnionOneofCase.Mul -> x.Mul |> ConvertDomain.FromProtobuf
        | ProtoClasses.Domain.Op.UnionOneofCase.Div -> x.Div |> ConvertDomain.FromProtobuf
        | ProtoClasses.Domain.Op.UnionOneofCase.Ln -> Domain.Op.Ln(x.Ln |> ConvertDomain.FromProtobuf)
        | ProtoClasses.Domain.Op.UnionOneofCase.Quantum -> x.Quantum |> ConvertDomain.FromProtobuf
        | ProtoClasses.Domain.Op.UnionOneofCase.Zero -> Domain.Op.Zero
        | _ -> Domain.Op.Unknown
    static member ToProtobuf (x:Domain.Op) : ProtoClasses.Domain.Op =
        let y = ProtoClasses.Domain.Op()
        match x with
        | Domain.Op.Val (p1) ->
            y.Val <- p1
        | Domain.Op.Sum (p1,p2) -> y.Sum <- ConvertDomain.OpCaseSumToProtobuf(p1,p2)
        | Domain.Op.Mul (p1,p2) -> y.Mul <- ConvertDomain.OpCaseMulToProtobuf(p1,p2)
        | Domain.Op.Div (p1,p2) -> y.Div <- ConvertDomain.OpCaseDivToProtobuf(p1,p2)
        | Domain.Op.Ln (p1) ->
            y.Ln <- p1 |> ConvertDomain.ToProtobuf
        | Domain.Op.Quantum (p1,p2,p3) -> y.Quantum <- ConvertDomain.OpCaseQuantumToProtobuf(p1,p2,p3)
        | Domain.Op.Zero -> y.Zero <- true
        | Domain.Op.Unknown -> ()
        y
    static member FromProtobuf (x:ProtoClasses.Domain.Op__Sum)  =
        Domain.Op.Sum
            ((x.P1 |> ConvertDomain.FromProtobuf),(x.P2 |> ConvertDomain.FromProtobuf))
    static member OpCaseSumToProtobuf (p1,p2) : ProtoClasses.Domain.Op__Sum =
        let y = ProtoClasses.Domain.Op__Sum()
        y.P1 <- p1 |> ConvertDomain.ToProtobuf
        y.P2 <- p2 |> ConvertDomain.ToProtobuf
        y
    static member FromProtobuf (x:ProtoClasses.Domain.Op__Mul)  =
        Domain.Op.Mul
            ((x.P1 |> ConvertDomain.FromProtobuf),(x.P2 |> ConvertDomain.FromProtobuf))
    static member OpCaseMulToProtobuf (p1,p2) : ProtoClasses.Domain.Op__Mul =
        let y = ProtoClasses.Domain.Op__Mul()
        y.P1 <- p1 |> ConvertDomain.ToProtobuf
        y.P2 <- p2 |> ConvertDomain.ToProtobuf
        y
    static member FromProtobuf (x:ProtoClasses.Domain.Op__Div)  =
        Domain.Op.Div
            ((x.P1 |> ConvertDomain.FromProtobuf),(x.P2 |> ConvertDomain.FromProtobuf))
    static member OpCaseDivToProtobuf (p1,p2) : ProtoClasses.Domain.Op__Div =
        let y = ProtoClasses.Domain.Op__Div()
        y.P1 <- p1 |> ConvertDomain.ToProtobuf
        y.P2 <- p2 |> ConvertDomain.ToProtobuf
        y
    static member FromProtobuf (x:ProtoClasses.Domain.Op__Quantum)  =
        Domain.Op.Quantum
            ((x.P1 |> ConvertDomain.FromProtobuf),(x.P2 |> ConvertDomain.FromProtobuf),(x.P3))
    static member OpCaseQuantumToProtobuf (p1,p2,p3) : ProtoClasses.Domain.Op__Quantum =
        let y = ProtoClasses.Domain.Op__Quantum()
        y.P1 <- p1 |> ConvertDomain.ToProtobuf
        y.P2 <- p2 |> ConvertDomain.ToProtobuf
        y.P3 <- p3
        y
    static member FromProtobuf (x:ProtoClasses.Domain.OpError) : Domain.OpError =
        match x.UnionCase with
        | ProtoClasses.Domain.OpError.UnionOneofCase.General -> Domain.OpError.General(x.General)
        | ProtoClasses.Domain.OpError.UnionOneofCase.DivisionByZero -> Domain.OpError.DivisionByZero
        | ProtoClasses.Domain.OpError.UnionOneofCase.NotSupported -> Domain.OpError.NotSupported
        | _ -> Domain.OpError.Unknown
    static member ToProtobuf (x:Domain.OpError) : ProtoClasses.Domain.OpError =
        let y = ProtoClasses.Domain.OpError()
        match x with
        | Domain.OpError.General (p1) ->
            y.General <- p1
        | Domain.OpError.DivisionByZero -> y.DivisionByZero <- true
        | Domain.OpError.NotSupported -> y.NotSupported <- true
        | Domain.OpError.Unknown -> ()
        y
    static member FromProtobuf (x:ProtoClasses.Domain.OpResult) : Domain.OpResult =
        match x.UnionCase with
        | ProtoClasses.Domain.OpResult.UnionOneofCase.Success -> Domain.OpResult.Success(x.Success)
        | ProtoClasses.Domain.OpResult.UnionOneofCase.Fail -> Domain.OpResult.Fail(x.Fail |> ConvertDomain.FromProtobuf)
        | _ -> Domain.OpResult.Unknown
    static member ToProtobuf (x:Domain.OpResult) : ProtoClasses.Domain.OpResult =
        let y = ProtoClasses.Domain.OpResult()
        match x with
        | Domain.OpResult.Success (p1) ->
            y.Success <- p1
        | Domain.OpResult.Fail (p1) ->
            y.Fail <- p1 |> ConvertDomain.ToProtobuf
        | Domain.OpResult.Unknown -> ()
        y
    static member FromProtobuf (x:ProtoClasses.Domain.Request) : Domain.Request =
        {
            Token = x.Token
            Operation = x.Operation |> ConvertDomain.FromProtobuf
        }
    static member ToProtobuf (x:Domain.Request) : ProtoClasses.Domain.Request =
        let y = ProtoClasses.Domain.Request()
        y.Token <- x.Token
        y.Operation <- x.Operation |> ConvertDomain.ToProtobuf
        y
    static member FromProtobuf (x:ProtoClasses.Domain.Response) : Domain.Response =
        {
            Token = x.Token
            Result = x.Result |> ConvertDomain.FromProtobuf
            ExecutionTime = x.ExecutionTime |> fun v -> v.ToTimeSpan()
            Extra = if x.ExtraCase = ProtoClasses.Domain.Response.ExtraOneofCase.ExtraValue then Some (x.ExtraValue) else None
            Since = x.Since |> fun v -> v.ToDateTimeOffset()
            Tags = x.Tags |> Seq.map(fun pair -> pair.Key,pair.Value) |> Map.ofSeq
        }
    static member ToProtobuf (x:Domain.Response) : ProtoClasses.Domain.Response =
        let y = ProtoClasses.Domain.Response()
        y.Token <- x.Token
        y.Result <- x.Result |> ConvertDomain.ToProtobuf
        y.ExecutionTime <- x.ExecutionTime |> Google.Protobuf.WellKnownTypes.Duration.FromTimeSpan
        match x.Extra with
        | Some v -> y.ExtraValue <- v
        | None -> ()
        y.Since <- x.Since |> Google.Protobuf.WellKnownTypes.Timestamp.FromDateTimeOffset
        y.Tags.Add(x.Tags)
        y
