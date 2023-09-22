open System

#load "../../make.Shared.fsx"
open Make.Shared


let protokeepFileName = "domain.protokeep"
let protoClassesDir = "./ProtoClasses/"
let protoFileName = protoClassesDir + "domain.proto"
let fsharpTypesFile = "./App/Domain.fs"
let fsharpProtoConvertersFile = "./App/DomainProtoConverters.fs"
let fsharpJsonConvertersFile = "./App/DomainJsonConverters.fs"
let fsharpCommonsFile = "./App/Protokeep.fs"

let gen _ =
    result {
        do! protokeep $"{protokeepFileName} lock" __SOURCE_DIRECTORY__
        do! protokeep $"{protokeepFileName} proto -o {protoFileName}" __SOURCE_DIRECTORY__

        do!
            protokeep
                $"{protokeepFileName} fsharp-types -o {fsharpTypesFile} --update-commons-in {fsharpCommonsFile}"
                __SOURCE_DIRECTORY__

        do!
            protokeep
                $"{protokeepFileName} fsharp-proto-converters -o {fsharpProtoConvertersFile} --update-commons-in {fsharpCommonsFile}"
                __SOURCE_DIRECTORY__

        do!
            protokeep
                $"{protokeepFileName} fsharp-json-converters -o {fsharpJsonConvertersFile} --update-commons-in {fsharpCommonsFile}"
                __SOURCE_DIRECTORY__
    }

let clean _ =
    result {
        do! dotnet "clean" __SOURCE_DIRECTORY__
        do! rm $"{protokeepFileName}.lock" __SOURCE_DIRECTORY__
        do! rm protoFileName __SOURCE_DIRECTORY__
        do! rm fsharpTypesFile __SOURCE_DIRECTORY__
        do! rm fsharpProtoConvertersFile __SOURCE_DIRECTORY__
        do! rm fsharpJsonConvertersFile __SOURCE_DIRECTORY__
        do! rm fsharpCommonsFile __SOURCE_DIRECTORY__
    }

[ ("CLEAN", clean)
  ("GEN", gen)
  ("BUILD", (fun _ -> dotnet "build" __SOURCE_DIRECTORY__))
  ("RUN", (fun _ -> dotnet "run --project App" __SOURCE_DIRECTORY__)) ]
|> make "Protokeep - Fsharp Console Example"
