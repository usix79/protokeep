#load "../../shared.fsx"

open FsToolkit.ErrorHandling
open Shared


let protokeepFileName = "domain.protokeep"
let protoClassesDir = "./ProtoClasses/"
let protoFileName = protoClassesDir + "domain.proto"
let fsharpTypesFile = "./App/Domain.fs"
let fsharpProtoFile = "./App/DomainProto.fs"
let fsharpJsonFile = "./App/DomainJson.fs"
let fsharpCommonsFile = "./App/Protokeep.fs"

let gen _ =
    result {
        do! protokeep $"{protokeepFileName} lock" __SOURCE_DIRECTORY__
        do! protokeep $"{protokeepFileName} proto -o {protoFileName}" __SOURCE_DIRECTORY__
        do! protokeep $"{protokeepFileName} fsharp-types -o {fsharpTypesFile} --update-commons" __SOURCE_DIRECTORY__
        do! protokeep $"{protokeepFileName} fsharp-proto -o {fsharpProtoFile} --update-commons" __SOURCE_DIRECTORY__
        do! protokeep $"{protokeepFileName} fsharp-json -o {fsharpJsonFile} --update-commons" __SOURCE_DIRECTORY__
    }

let clean _ =
    result {
        do! dotnet "clean" __SOURCE_DIRECTORY__
        do! rm $"{protokeepFileName}.lock" __SOURCE_DIRECTORY__
        do! rm protoFileName __SOURCE_DIRECTORY__
        do! rm fsharpTypesFile __SOURCE_DIRECTORY__
        do! rm fsharpProtoFile __SOURCE_DIRECTORY__
        do! rm fsharpJsonFile __SOURCE_DIRECTORY__
        do! rm fsharpCommonsFile __SOURCE_DIRECTORY__
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
  ("RUN", (fun _ -> dotnet "run --project App" __SOURCE_DIRECTORY__)) ]
|> make "Protokeep - Fsharp Console Example"
