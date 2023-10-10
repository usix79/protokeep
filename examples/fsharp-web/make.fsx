#load "../../shared.fsx"

open System.Threading.Tasks
open FsToolkit.ErrorHandling
open Shared


let protokeepDir = "./schema/"
let inputFiles = [ "Domain"; "Subdomain" ]
let serverDir = "./server/"
let clientDir = "./client/"

let protokeep cmd =
    dotnet $"run --project ../../src/ --framework net7.0 {cmd}" __SOURCE_DIRECTORY__

let gen _ =
    result {
        for file in inputFiles do
            let fullFileName = $"{protokeepDir}{file}.protokeep"
            do! protokeep $"{fullFileName} lock"

            do! protokeep $"{fullFileName} fsharp-types -o {serverDir}{file}.g.fs --update-commons"
            do! protokeep $"{fullFileName} fsharp-types -o {clientDir}{file}.g.fs --update-commons"

            do!
                protokeep
                    $"{fullFileName} fsharp-json -o {serverDir}{file}Json.g.fs --update-commons -ns Domain.JsonConverters"

            do!
                protokeep
                    $"{fullFileName} fsharp-fable -o {clientDir}{file}Json.g.fs --update-commons -ns Domain.JsonConverters"

    }

let build _ =
    result {
        do! dotnet "build" __SOURCE_DIRECTORY__
        do! dotnet $"fable ./client/client.fsproj --noCache" __SOURCE_DIRECTORY__
    }

let clean _ =
    result {
        do! dotnet "clean" __SOURCE_DIRECTORY__

        do! rmAll $"{__SOURCE_DIRECTORY__}/{protokeepDir}" "*.lock"
        do! rmAll $"{__SOURCE_DIRECTORY__}/{serverDir}" "*.g.fs"
        do! rmAll $"{__SOURCE_DIRECTORY__}/{clientDir}" "*.g.fs"
        do! rmAll $"{__SOURCE_DIRECTORY__}/{clientDir}" "*.js"
    }

let rebuild _ =
    result {
        do! clean ()
        do! gen ()
        do! build ()
    }

let run _ =
    result {
        [| task { return dotnet "watch --project ./server/ run" __SOURCE_DIRECTORY__ }
           task { return dotnet "fable watch client" __SOURCE_DIRECTORY__ } |]
        |> Array.map (fun t -> t :> Task)
        |> Task.WaitAll
    }


[ ("CLEAN", clean)
  ("GEN", gen)
  ("BUILD", build)
  ("REBUILD", rebuild)
  ("RUN", run) ]
|> make "Protokeep - Fsharp Console Example"
