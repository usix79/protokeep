// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open ProtoClasses.Domain

// Define a function to construct a message to print
[<EntryPoint>]
let main argv =
    let crossroad = Crossroad()
    crossroad.Id <- 124
    crossroad.Street1 <- "Bond st"
    crossroad.Street2 <- "Oxford st"

    printfn "Crossroad: %A" crossroad
    0 // return an integer exit code
