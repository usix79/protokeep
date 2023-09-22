#load "./make.Shared.fsx"
open Make.Shared

let pack () =
    result {
        do! dotnet "build -c Release" "."
        do! dotnet "pack -c Release" "."
        do! dotnet "tool uninstall --local Protokeep" "."
        do! dotnet "tool install --local --add-source ./nupkg Protokeep" "."
    }

[ ("CLEAN", (fun _ -> dotnet "clean" "."))
  ("BUILD", (fun _ -> dotnet "build" "."))
  ("PACK", (fun _ -> pack ())) ]
|> make "Protokeeper Make Tool"
