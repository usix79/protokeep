namespace Protokeep

module FsharpTypes =

    type Key =
        | Value of string
        | Items of Key list
        | Inner of Key

        member x.Stringify() =
            match x with
            | Value v -> v
            | Items keys -> keys |> List.map (fun key -> key.Stringify()) |> String.concat ","
            | Inner key -> $"({key.Stringify()})"

    let (|TryFind|_|) f key = f key


module FsharpJsonConvertersHelpers =

    open System.Text.Json
    type FromJsonDelegate<'a> = delegate of byref<Utf8JsonReader> -> 'a
    type ToJsonDelegate<'a> = delegate of inref<Utf8JsonWriter> * 'a -> unit

    let fromDateTime (v: System.DateTime) = v.ToString("O")

    let durationRegex =
        System.Text.RegularExpressions.Regex @"^(-)?([0-9]{1,12})(\.[0-9]{1,9})?s$"

    let subsecondScalingFactors =
        [| 0
           100000000
           100000000
           10000000
           1000000
           100000
           10000
           1000
           100
           10
           1 |]

    let toTimeSpan (v: string) =
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

                    parsedFraction * (subsecondScalingFactors.[subseconds.Length]) / 1000000
                    |> float
                else
                    0.

            System.TimeSpan.FromMilliseconds(sign * (seconds * 1000. + milliseconds))
        | false -> failwithf "Invalid Duration value: %s" v

    let fromTimeSpan (v: System.TimeSpan) =
        sprintf "%d.%ds" (int64 v.TotalSeconds) v.Milliseconds
