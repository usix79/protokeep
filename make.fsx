#load "./shared.fsx"

open System
open System.IO
open Shared
open FsToolkit.ErrorHandling

let protokeep cmd =
    dotnet $"run --project src/ --framework net7.0 {cmd}" __SOURCE_DIRECTORY__

let pack () =
    result {
        do! dotnet "build -c Release" "."
        do! dotnet "pack -c Release" "."
        do! dotnet "tool uninstall --local Protokeep" "."
        do! dotnet "tool install --local --add-source ./nupkg Protokeep" "."
    }

let gen () =
    result {
        do! dotnet "fsi ./examples/complex-domain/make.fsx rebuild" "."
        do! dotnet "fsi ./examples/console-fsharp/make.fsx rebuild" "."
        do! dotnet "fsi ./examples/web-fsharp/make.fsx rebuild" "."
    }

let case (caseName: string) =
    let caseName = caseName.ToLowerInvariant()

    result {
        let dir = Path.Combine(__SOURCE_DIRECTORY__, "tests/cases")
        let fileName = Path.Combine(dir, $"{caseName}.schema.case")

        do! protokeep $"{fileName} lock"
        do! protokeep $"{fileName} proto -o {dir}/{caseName}.proto.case"
        do! protokeep $"{fileName} fsharp-types -o {dir}/{caseName}.fsharp-types.case"
        do! protokeep $"{fileName} fsharp-proto -o {dir}/{caseName}.fsharp-proto.case -ns Test.Converters"
        do! protokeep $"{fileName} fsharp-json -o {dir}/{caseName}.fsharp-json.case -ns Test.Converters"
        do! protokeep $"{fileName} fsharp-mongo -o {dir}/{caseName}.fsharp-mongo.case -ns Test.Converters"
        do! protokeep $"{fileName} fsharp-fable -o {dir}/{caseName}.fsharp-fable.case -ns Test.Converters"
    }



[ ("CLEAN", (fun _ -> dotnet "clean" "."))
  ("BUILD", (fun _ -> dotnet "build" "."))
  ("PACK", (fun _ -> pack ()))
  ("GEN", (fun _ -> gen ()))
  ("CASE", case) ]
|> make "Protokeeper Make Tool"
