namespace Test.Converters

open Fable.SimpleJson
open Protokeep

type ConvertTestDomain () =
    static member IncidentFromJson (json: Json): Test.Domain.Incident =
        let mutable y = Test.Domain.Incident.Unknown
        FsharpFableHelpers.getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "SwitchedOff" -> pair.Value |> FsharpFableHelpers.ifBool (fun v -> y <- Test.Domain.Incident.SwitchedOff)
            | "MissedTurns" ->
                let mutable _count = 0
                pair.Value |> FsharpFableHelpers.ifNumber (fun v -> _count <- v |> unbox)
                y <- _count |> Test.Domain.Incident.MissedTurns
            | "Delayes" -> pair.Value |> (fun v -> y <- v |> ConvertTestDomain.IncidentCaseDelayesFromJson)
            | "Root" ->
                let mutable _p1 = ResizeArray()
                pair.Value |> FsharpFableHelpers.ifArray (Seq.iter ((fun v -> v |> ConvertTestDomain.IncidentFromJson |> _p1.Add)))
                y <- _p1 |> List.ofSeq |> Test.Domain.Incident.Root
            | "Noise" ->
                let mutable _p1 = ResizeArray()
                pair.Value |> FsharpFableHelpers.ifMap (fun (key, value) -> key |> FsharpFableHelpers.ifString (fun v -> v |> fun k -> value |> FsharpFableHelpers.ifString (fun v -> v |> fun v -> _p1.Add(k, v))))
                y <- _p1 |> Map.ofSeq |> Test.Domain.Incident.Noise
            | _ -> () )
        y
    static member IncidentToJson (x:Test.Domain.Incident) =
        match x with
        | Test.Domain.Incident.SwitchedOff -> "SwitchedOff", JBool (true)
        | Test.Domain.Incident.MissedTurns (count) -> "MissedTurns", JNumber (unbox count)
        | Test.Domain.Incident.Delayes (from,to) -> "Delayes", ConvertTestDomain.IncidentCaseDelayesToJson (from,to)
        | Test.Domain.Incident.Root (p1) -> "Root", JArray (p1 |> Seq.map (fun v -> (v |> ConvertTestDomain.IncidentToJson)) |> List.ofSeq)
        | Test.Domain.Incident.Noise (p1) -> "Noise", p1 |> FsharpFableHelpers.mapToArray (fun k -> JString (k)) (fun v -> JString (v))
        | _ -> "Unknown", JBool (true)
        |> List.singleton |> Map.ofList |> JObject
    static member IncidentCaseDelayesFromJson (json: Json) =
        let mutable from = System.DateTime.MinValue
        let mutable to = System.DateTime.MinValue
        FsharpFableHelpers.getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "From" -> pair.Value |> FsharpFableHelpers.ifString (fun v -> from <- v |> FsharpFableHelpers.toDateTime)
            | "To" -> pair.Value |> FsharpFableHelpers.ifString (fun v -> to <- v |> FsharpFableHelpers.toDateTime)
            | _ -> () )
        Test.Domain.Incident.Delayes (from,to)
    static member IncidentCaseDelayesToJson (from,to) =
        [
            "From", JString (from |> FsharpFableHelpers.fromDateTime)
            "To", JString (to |> FsharpFableHelpers.fromDateTime)
        ] |> Map.ofList |> JObject
