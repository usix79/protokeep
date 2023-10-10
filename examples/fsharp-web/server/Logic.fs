module Logic

open Domain

let bind2 f1 arg1 f2 arg2 c =
    f1 arg1 |> Result.bind (fun v1 -> f2 arg2 |> Result.bind (fun v2 -> c v1 v2))

let rec calc =
    function
    | Val v -> Ok v
    | Sum(op1, op2) -> bind2 calc op1 calc op2 (fun v1 v2 -> (+) v1 v2 |> Ok)
    | Mul(op1, op2) -> bind2 calc op1 calc op2 (fun v1 v2 -> (*) v1 v2 |> Ok)
    | Div(op1, op2) ->
        bind2 calc op1 calc op2 (fun v1 v2 ->
            if v2 <> 0 then
                (/) v1 v2 |> Ok
            else
                Error OpError.DivisionByZero)
    | Ln op -> calc op |> Result.map (float >> System.Math.Log >> int)
    | Quantum _ -> Error OpError.NotSupported
    | Op.Unknown -> OpError.General [ "Unknown operation"; "Look in source" ] |> Error
    | Zero -> Ok 0
    | Imagine(ValueSome i) -> Ok i
    | Imagine ValueNone -> Ok 0
    | SumAll ops ->
        ops
        |> List.map calc
        |> List.fold
            (fun (sum, errors) v ->
                match v with
                | Ok v -> (sum + v), errors
                | Error err -> sum, (string (err) :: errors))
            (0, [])
        |> (fun (sum, errors) ->
            match errors with
            | [] -> Ok sum
            | _ -> Error(OpError.General errors))
