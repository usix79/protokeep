#load "../../shared.fsx"

open FsToolkit.ErrorHandling
open Shared


let protokeepDir = "./protokeep/"

let inputFiles = [ "Betting"; "BettingFootball" ] |> List.map (fun x -> $"{x}")

let protoDir = "./proto/"
let fsharpDir = "./fsharp/"
let fableDir = "./fable/"

let gen _ =
    result {
        for file in inputFiles do
            let fullFileName = $"{protokeepDir}{file}.protokeep"
            do! protokeep $"{fullFileName} lock" __SOURCE_DIRECTORY__
            do! protokeep $"{fullFileName} proto -o {protoDir}{file}.proto" __SOURCE_DIRECTORY__

            do! protokeep $"{fullFileName} fsharp-types -o {fsharpDir}{file}.fs --update-commons" __SOURCE_DIRECTORY__

            do!
                protokeep
                    $"{fullFileName} fsharp-proto -o {fsharpDir}{file}Proto.fs --update-commons"
                    __SOURCE_DIRECTORY__

            do!
                protokeep
                    $"{fullFileName} fsharp-json -o {fsharpDir}{file}Json.fs --update-commons"
                    __SOURCE_DIRECTORY__

            do!
                protokeep
                    $"{fullFileName} fsharp-fable -o {fableDir}{file}Fable.fs --update-commons"
                    __SOURCE_DIRECTORY__

    }

let clean _ =
    result {
        for file in inputFiles do
            do! rm $"{protokeepDir}{file}.protokeep.lock" __SOURCE_DIRECTORY__

        do! rmAll $"{__SOURCE_DIRECTORY__}/{protoDir}"
        do! rmAll $"{__SOURCE_DIRECTORY__}/{fsharpDir}"
        do! rmAll $"{__SOURCE_DIRECTORY__}/{fableDir}"
    }

let rebuild _ =
    result {
        do! clean ()
        do! gen ()
    }

[ ("CLEAN", clean); ("GEN", gen); ("REBUILD", rebuild) ]
|> make "Protokeep - Complex Domain Example"
