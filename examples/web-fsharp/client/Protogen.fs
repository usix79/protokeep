namespace Protogen
module FableConverterHelpers =

    open Fable.SimpleJson

    let getProps = function JObject p -> p | _ -> Map.empty
    let ifBool action = function (JBool v) -> action v | _ -> ()
    let ifString action = function (JString v) -> action v | _ -> ()
    let ifNumber action = function (JNumber v) -> action v | _ -> ()
    let ifObject action = function (JObject v) -> action v | _ -> ()
    let ifArray action = function (JArray v) -> action v | _ -> ()

    let toDateTimeOffset (v:string) = System.DateTimeOffset.Parse v
    let fromDateTimeOffset (v:System.DateTimeOffset) = v.ToString("O")

    let durationRegex = System.Text.RegularExpressions.Regex @"^(-)?([0-9]{1,12})(\.[0-9]{1,9})?s$"
    let subsecondScalingFactors = [| 0; 100000000; 100000000; 10000000; 1000000; 100000; 10000; 1000; 100; 10; 1 |]
    let toTimeSpan (v:string) =
            let m = durationRegex.Match(v)
            match m.Success with
            | true ->
                let signText = m.Groups.[1].Value
                let secondsText = m.Groups.[2].Value
                let subseconds = m.Groups.[3].Value
                let sign = if signText = "-" then -1. else 1.

                let seconds = System.Int64.Parse(secondsText) |> float
                let milliseconds =
                    if subseconds <> "" then
                        let parsedFraction = System.Int32.Parse(subseconds.Substring(1))
                        parsedFraction * (subsecondScalingFactors.[subseconds.Length]) / 1000000 |> float
                    else 0.

                System.TimeSpan.FromMilliseconds(sign * (seconds * 1000. + milliseconds))
            | false -> failwithf "Invalid Duration value: %s"  v

    let fromTimeSpan (v:System.TimeSpan) =
        sprintf "%d.%ds" (int64 v.TotalSeconds) v.Milliseconds

module FsharpTypes =

    type Key =
        | Value of string
        | Items of Key list
        | Inner of Key

