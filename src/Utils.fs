module Protogen.Utils

let rec traverse f list =

    let apply fResult xResult =
        match fResult,xResult with
        | Ok f, Ok x -> f x |> Ok
        | Error errs, Ok _ -> Error errs
        | Ok f, Error errs -> Error errs
        | Error errs1, Error errs2 -> Error (errs1 @ errs2)

    let (<*>) = apply
    let cons head tail = head :: tail

    match list with
    | [] -> Ok []
    | head::tail -> Ok cons <*> (f head) <*> (traverse f tail)


let tryMap items =
    let rec f = function
    | [] -> Map.empty, []
    | (key,value)::tail ->
        let map, errors = f tail
        if map.ContainsKey key then map, (key :: errors)
        else map.Add(key, value), errors

    let map, errors = f items

    if errors.IsEmpty then Ok map else Error errors