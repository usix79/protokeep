open System.IO
open System.Threading.Tasks

#load "../../make.Shared.fsx"
open Make.Shared


let protokeepDir = "./domain/"
let inputFiles = [ "Domain"; "Subdomain" ]
let serverDir = "./server/gen/"
let clientDir = "./client/gen/"

let gen _ =
    result {
        for file in inputFiles do
            let fullFileName = $"{protokeepDir}{file}.protokeep"
            do! protokeep $"{fullFileName} lock" __SOURCE_DIRECTORY__

            do! protokeep $"{fullFileName} fsharp-types -o {serverDir}{file}.fs --update-common" __SOURCE_DIRECTORY__
            do! protokeep $"{fullFileName} fsharp-types -o {clientDir}{file}.fs --update-commons" __SOURCE_DIRECTORY__

            do!
                protokeep
                    $"{fullFileName} fsharp-json-converters -o {serverDir}{file}Converters.fs --update-commons -ns Domain.JsonConverters"
                    __SOURCE_DIRECTORY__

            do!
                protokeep
                    $"{fullFileName} fable-converters -o {clientDir}{file}Converters.fs --update-commons -ns Domain.JsonConverters"
                    __SOURCE_DIRECTORY__
    }

let build _ =
    result {
        do! dotnet "build" __SOURCE_DIRECTORY__
        do! dotnet $"fable ./client/client.fsproj" __SOURCE_DIRECTORY__
    }

let clean _ =
    result {
        do! dotnet "clean" __SOURCE_DIRECTORY__

        for file in inputFiles do
            do! rm $"{protokeepDir}{file}.protokeep.lock" __SOURCE_DIRECTORY__

        do! rmAll $"{__SOURCE_DIRECTORY__}/{serverDir}"
        do! rmAll $"{__SOURCE_DIRECTORY__}/{clientDir}"
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
