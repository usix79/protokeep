namespace Domain.Subdomain
open Protogen.FsharpTypes
type Status =
    | Unknown = 0
    | Green = 1
    | Yellow = 2
    | Red = 3
