#load "../../shared.fsx"

open FsToolkit.ErrorHandling
open Shared


let protokeepFileName = "./Domain.protokeep"
let fsharpTypesFile = "./Domain.fs"
let fsharpMongoFile = "./DomainMongoConverters.fs"
let fsharpJsonConvertersFile = "./DomainJsonConverters.fs"

let gen _ =
    result {
        do! protokeep $"{protokeepFileName} lock" __SOURCE_DIRECTORY__

        do! protokeep $"{protokeepFileName} fsharp-types -o {fsharpTypesFile} --update-commons" __SOURCE_DIRECTORY__

        do!
            protokeep
                $"{protokeepFileName} fsharp-json-converters -o {fsharpJsonConvertersFile} --update-commons"
                __SOURCE_DIRECTORY__

        do! protokeep $"{protokeepFileName} fsharp-mongo -o {fsharpMongoFile} --update-commons" __SOURCE_DIRECTORY__

    }

let clean _ =
    result {
        do! dotnet "clean" __SOURCE_DIRECTORY__
        do! rm $"{protokeepFileName}.lock" __SOURCE_DIRECTORY__
        do! rm fsharpTypesFile __SOURCE_DIRECTORY__
        do! rm fsharpMongoFile __SOURCE_DIRECTORY__
    }

let rebuild _ =
    result {
        do! clean ()
        do! gen ()
    }


[ ("CLEAN", clean)
  ("GEN", gen)
  ("BUILD", (fun _ -> dotnet "build" __SOURCE_DIRECTORY__))
  ("REBUILD", rebuild)
  ("RUN", (fun _ -> dotnet "run --project App" __SOURCE_DIRECTORY__)) ]
|> make "Protokeep - Fsharp Console Example"
