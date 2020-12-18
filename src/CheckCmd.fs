[<RequireQualifiedAccess>]
module Protogen.CheckCmd

open System
open Types

let Handler module' locks typesCache args =
    Types.lock module' locks typesCache
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
