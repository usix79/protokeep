module rec Domain
open Protogen.FsharpTypes
type Op =
    | Unknown
    | Val of p1:int
    | Sum of p1:Domain.Op*p2:Domain.Op
    | Mul of p1:Domain.Op*p2:Domain.Op
    | Div of p1:Domain.Op*p2:Domain.Op
    | Ln of p1:Domain.Op
    | Quantum of p1:Domain.Op*p2:Domain.Op*p3:string
    | Zero
with
    static member MakeUnknownKey () = Key.Value "0"
    static member MakeValKey () = Key.Value "1"
    static member MakeSumKey () = Key.Value "2"
    static member MakeMulKey () = Key.Value "3"
    static member MakeDivKey () = Key.Value "4"
    static member MakeLnKey () = Key.Value "5"
    static member MakeQuantumKey () = Key.Value "6"
    static member MakeZeroKey () = Key.Value "7"
    member x.Key =
        match x with
        | Unknown -> Op.MakeUnknownKey ()
        | Val (p1') -> Op.MakeValKey ()
        | Sum (p1', p2') -> Op.MakeSumKey ()
        | Mul (p1', p2') -> Op.MakeMulKey ()
        | Div (p1', p2') -> Op.MakeDivKey ()
        | Ln (p1') -> Op.MakeLnKey ()
        | Quantum (p1', p2', p3') -> Op.MakeQuantumKey ()
        | Zero -> Op.MakeZeroKey ()
type OpError =
    | Unknown
    | General of p1:string
    | DivisionByZero
    | NotSupported
with
    static member MakeUnknownKey () = Key.Value "0"
    static member MakeGeneralKey () = Key.Value "1"
    static member MakeDivisionByZeroKey () = Key.Value "2"
    static member MakeNotSupportedKey () = Key.Value "3"
    member x.Key =
        match x with
        | Unknown -> OpError.MakeUnknownKey ()
        | General (p1') -> OpError.MakeGeneralKey ()
        | DivisionByZero -> OpError.MakeDivisionByZeroKey ()
        | NotSupported -> OpError.MakeNotSupportedKey ()
type OpResult =
    | Unknown
    | Success of p1:int
    | Fail of p1:Domain.OpError
with
    static member MakeUnknownKey () = Key.Value "0"
    static member MakeSuccessKey () = Key.Value "1"
    static member MakeFailKey () = Key.Value "2"
    member x.Key =
        match x with
        | Unknown -> OpResult.MakeUnknownKey ()
        | Success (p1') -> OpResult.MakeSuccessKey ()
        | Fail (p1') -> OpResult.MakeFailKey ()
type Request = {
    Token : string
    Operation : Domain.Op
}
type Response = {
    Token : string
    Result : Domain.OpResult
    ExecutionTime : System.TimeSpan
    Extra : string option
    Since : System.DateTimeOffset
    Tags : Map<string,string>
}
