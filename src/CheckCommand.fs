[<RequireQualifiedAccess>]
module Protogen.CheckCommand

open System
open Types

let Handler modules locks args =
    Evolution.lock modules locks
    |> Result.mapError (sprintf "%A")
    |> Result.bind(fun newlocks ->
        if newlocks <> locks then Console.WriteLine("Check Ok")
        else Console.WriteLine("Check Ok, nothing changed")
        Ok ())

let Instance = {
    Name = "check"
    Description = "check if it is possible to lock given pgen types"
    Run = Handler
}
