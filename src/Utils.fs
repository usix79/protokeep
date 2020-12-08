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


let tryMap keyf valf items =
    let rec f = function
    | [] -> Map.empty, []
    | head::tail ->
        let map, errors = f tail
        let key = keyf head
        if map.ContainsKey key then map, (key :: errors)
        else map.Add(key, valf head), errors

    let map, errors = f items

    if errors.IsEmpty then Ok map else Error errors

let checkMissedItems keyf duplicateError missedError oldItems newItems =
    newItems
    |> tryMap keyf id
    |> Result.mapError (List.map duplicateError)
    |> Result.bind (fun newItemsMap ->
        let missedFields =
            oldItems
            |> List.choose (fun x ->
                let fieldName = keyf x
                match newItemsMap.TryFind fieldName with
                | Some _ -> None
                | None -> Some fieldName)

        if missedFields.IsEmpty then Ok newItems
        else missedFields |> List.map missedError |> Error)
