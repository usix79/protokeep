#load "./shared.fsx"

open Shared
open FsToolkit.ErrorHandling

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


[ ("CLEAN", (fun _ -> dotnet "clean" "."))
  ("BUILD", (fun _ -> dotnet "build" "."))
  ("PACK", (fun _ -> pack ()))
  ("GEN", (fun _ -> gen ())) ]
|> make "Protokeeper Make Tool"
