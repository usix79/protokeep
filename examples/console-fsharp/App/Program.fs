open ProtoClasses.Domain

[<EntryPoint>]
let main argv =
    let crossroad = Crossroad()
    crossroad.Id <- 124
    crossroad.Street1 <- "Bond st"
    crossroad.Street2 <- "Oxford st"

    printfn "Crossroad: %A" crossroad
    0 // return an integer exit code