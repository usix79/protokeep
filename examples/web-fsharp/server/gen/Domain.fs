namespace Domain

open Protokeep.FsharpTypes

type Op =
    | Unknown
    | Val of p1: int
    | Sum of p1: Op*p2: Op
    | Mul of p1: Op*p2: Op
    | Div of p1: Op*p2: Op
    | Ln of p1: Op
    | Quantum of p1: Op*p2: Op*p3: string
    | Imagine of p1: int voption
    | Zero

    static member MakeUnknownKey () = Key.Value "0"
    static member MakeValKey () = Key.Value "1"
    static member MakeSumKey () = Key.Value "2"
    static member MakeMulKey () = Key.Value "3"
    static member MakeDivKey () = Key.Value "4"
    static member MakeLnKey () = Key.Value "5"
    static member MakeQuantumKey () = Key.Value "6"
    static member MakeImagineKey () = Key.Value "7"
    static member MakeZeroKey () = Key.Value "8"
    interface IEntity with
        member x.Key =
            match x with
            | Op.Unknown -> Op.MakeUnknownKey ()
            | Op.Val (p1') -> Op.MakeValKey ()
            | Op.Sum (p1', p2') -> Op.MakeSumKey ()
            | Op.Mul (p1', p2') -> Op.MakeMulKey ()
            | Op.Div (p1', p2') -> Op.MakeDivKey ()
            | Op.Ln (p1') -> Op.MakeLnKey ()
            | Op.Quantum (p1', p2', p3') -> Op.MakeQuantumKey ()
            | Op.Imagine (p1') -> Op.MakeImagineKey ()
            | Op.Zero -> Op.MakeZeroKey ()


type OpError =
    | Unknown
    | General of p1: string
    | DivisionByZero
    | NotSupported

    static member MakeUnknownKey () = Key.Value "0"
    static member MakeGeneralKey () = Key.Value "1"
    static member MakeDivisionByZeroKey () = Key.Value "2"
    static member MakeNotSupportedKey () = Key.Value "3"
    interface IEntity with
        member x.Key =
            match x with
            | OpError.Unknown -> OpError.MakeUnknownKey ()
            | OpError.General (p1') -> OpError.MakeGeneralKey ()
            | OpError.DivisionByZero -> OpError.MakeDivisionByZeroKey ()
            | OpError.NotSupported -> OpError.MakeNotSupportedKey ()


type OpResult =
    | Unknown
    | Success of p1: int
    | Fail of p1: OpError

    static member MakeUnknownKey () = Key.Value "0"
    static member MakeSuccessKey () = Key.Value "1"
    static member MakeFailKey () = Key.Value "2"
    interface IEntity with
        member x.Key =
            match x with
            | OpResult.Unknown -> OpResult.MakeUnknownKey ()
            | OpResult.Success (p1') -> OpResult.MakeSuccessKey ()
            | OpResult.Fail (p1') -> OpResult.MakeFailKey ()


type Request = {
    Token : string
    Operation : Op
}
with
    static member Default: Lazy<Request> =
        lazy {
            Token = ""
            Operation = Op.Unknown
        }

type Response = {
    Token : string
    Result : OpResult
    ExecutionTime : System.TimeSpan
    Extra : string voption
    Since : System.DateTime
    Tags : Map<string,string>
    Status : Domain.Subdomain.Status
}
with
    static member Default: Lazy<Response> =
        lazy {
            Token = ""
            Result = OpResult.Unknown
            ExecutionTime = System.TimeSpan.Zero
            Extra = ValueNone
            Since = System.DateTime.MinValue
            Tags = Map.empty
            Status = Subdomain.Status.Unknown
        }

