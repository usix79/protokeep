module rec Domain
type Op =
    | Unknown
    | Val of p1:int
    | Sum of p1:Domain.Op*p2:Domain.Op
    | Mul of p1:Domain.Op*p2:Domain.Op
    | Div of p1:Domain.Op*p2:Domain.Op
    | Ln of p1:Domain.Op
    | Quantum of p1:Domain.Op*p2:Domain.Op*p3:string
type OpError =
    | Unknown
    | General of p1:string
    | DivisionByZero
    | NotSupported
type OpResult =
    | Unknown
    | Success of p1:int
    | Fail of p1:Domain.OpError
type Request = {
    Token : string
    Operation : Domain.Op
}
type Response = {
    Token : string
    Result : Domain.OpResult
    ExecutionTime : System.TimeSpan
    Extra : string option
}
