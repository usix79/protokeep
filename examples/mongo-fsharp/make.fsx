#load "../../shared.fsx"

open FsToolkit.ErrorHandling
open Shared


let protokeepFileName = "./Domain.protokeep"
let fsharpTypesFile = "./Domain.fs"
let fsharpMongoFile = "./DomainMongo.fs"

let gen _ =
    result {
        do! protokeep $"{protokeepFileName} lock" __SOURCE_DIRECTORY__

        do!
            protokeep
                $"{protokeepFileName} fsharp-types -o {fsharpTypesFile} --update-commons -ns Domain"
                __SOURCE_DIRECTORY__

        do!
            protokeep
                $"{protokeepFileName} fsharp-mongo -o {fsharpMongoFile} --update-commons -ns Domain"
                __SOURCE_DIRECTORY__
    }

let clean _ =
    result {
        do! dotnet "clean" __SOURCE_DIRECTORY__
        do! rm $"{protokeepFileName}.lock" __SOURCE_DIRECTORY__
        do! rm fsharpTypesFile __SOURCE_DIRECTORY__
        do! rm fsharpMongoFile __SOURCE_DIRECTORY__
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
  ("RUN", (fun _ -> dotnet "run" __SOURCE_DIRECTORY__)) ]
|> make "Protokeep - Fsharp Console Example"
