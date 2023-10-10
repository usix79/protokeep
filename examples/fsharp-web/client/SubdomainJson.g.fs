namespace Domain.JsonConverters

open Fable.SimpleJson
open Protokeep

type ConvertDomainSubdomain () =
    static member StatusFromString = function
        | "StatusGreen" -> Domain.Subdomain.Status.Green
        | "StatusYellow" -> Domain.Subdomain.Status.Yellow
        | "StatusRed" -> Domain.Subdomain.Status.Red
        | _ -> Domain.Subdomain.Status.Unknown
    static member StatusToString = function
        | Domain.Subdomain.Status.Green -> "StatusGreen"
        | Domain.Subdomain.Status.Yellow -> "StatusYellow"
        | Domain.Subdomain.Status.Red -> "StatusRed"
        | _ -> "Unknown"
