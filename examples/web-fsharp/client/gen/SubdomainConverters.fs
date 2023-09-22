namespace Protokeep.FableConverters
open Fable.SimpleJson
open Protokeep.FableConverterHelpers
type ConvertDomainSubdomain () =
    static member DefaultStatus =
        lazy Domain.Subdomain.Status.Unknown
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
