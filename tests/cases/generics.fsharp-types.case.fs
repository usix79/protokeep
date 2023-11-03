namespace Test.Domain

open Protokeep.FsharpTypes

type Game = {
    Units : Map<int,Set<string>>
}
with
    static member Default: Lazy<Game> =
        lazy {
            Units = Map.empty
        }

