#load "../../shared.fsx"

open FsToolkit.ErrorHandling
open Shared


let inputFiles =
    [ "Game"; "Betting"; "BettingFootball" ] |> List.map (fun x -> $"{x}")


let schemaDir = "./schema/"
let protoDir = "./proto/"
let domainDir = "./domain/"
let backendDir = "./backend/"

let protokeep cmd =
    dotnet $"run --project ../../src/ --framework net7.0 {cmd}" __SOURCE_DIRECTORY__

let rm fileName = rm fileName __SOURCE_DIRECTORY__

let rmAll dir pattern =
    rmAll (System.IO.Path.Combine(__SOURCE_DIRECTORY__, dir)) pattern

let gen _ =
    result {
        for file in inputFiles do
            let fileName = $"{schemaDir}{file}.protokeep"
            do! protokeep $"{fileName} lock"
            do! protokeep $"{fileName} proto -o {protoDir}Example.{file}.proto"
            do! protokeep $"{fileName} fsharp-types -o {domainDir}{file}.g.fs --update-commons"
            do! protokeep $"{fileName} fsharp-proto -o {backendDir}{file}ProtoConverters.g.fs --update-commons"
            do! protokeep $"{fileName} fsharp-json -o {backendDir}{file}JsonConverters.g.fs --update-commons"
            do! protokeep $"{fileName} fsharp-mongo -o {backendDir}{file}MongoConverters.g.fs --update-commons"
    }

let clean _ =
    result {
        do! dotnet "clean" __SOURCE_DIRECTORY__

        for file in inputFiles do
            let fileName = $"{schemaDir}{file}.protokeep"
            do! rm $"{fileName}.lock"

        do! rmAll protoDir "*.proto"
        do! rmAll domainDir "*.g.fs"
        do! rmAll backendDir "*.g.fs"
    }

let build _ = dotnet "build" __SOURCE_DIRECTORY__

let rebuild _ =
    result {
        do! clean ()
        do! gen ()
        do! build ()
    }

[ ("CLEAN", clean)
  ("GEN", gen)
  ("BUILD", build)
  ("REBUILD", rebuild)
  ("RUN", (fun _ -> dotnet "run --project backend" __SOURCE_DIRECTORY__)) ]
|> make "Protokeep - Fsharp Console Example"
