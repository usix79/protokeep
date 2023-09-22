open System

#load "../../make.Shared.fsx"
open Make.Shared


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
                    $"{fullFileName} fsharp-proto-converters -o {fsharpDir}{file}ProtoConverters.fs --update-commons"
                    __SOURCE_DIRECTORY__

            do!
                protokeep
                    $"{fullFileName} fsharp-json-converters -o {fsharpDir}{file}JsonConverters.fs --update-commons"
                    __SOURCE_DIRECTORY__

            do!
                protokeep
                    $"{fullFileName} fable-converters -o {fableDir}{file}FableConverters.fs --update-commons"
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

[ ("CLEAN", clean); ("GEN", gen) ] |> make "Protokeep - Complex Domain Example"
