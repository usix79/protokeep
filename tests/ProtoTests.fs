module ProtoTests

open System
open FSharp.Reflection
open Xunit
open Protokeep.Types
open Protokeep

open Utils

[<Theory; TestCasesFromFiles("ProtoTests", [| "Input"; "Output" |])>]
let ``All ProtoTests should pass`` (scenarioName, input, expectedOutput: string) =
    Parsers.parsePkDoc input
    |> Result.bind (fun module' ->
        let typesCache = (Types.toTypesCacheItems module' |> Map.ofList)

        Types.resolveReferences module' []
        |> Result.mapError (fun error -> failwithf "%A" error)
        |> Result.map (fun (module', typesCache) ->
            Types.lock module' (LocksCollection []) typesCache
            |> Result.map (fun locks ->
                let outputText = ProtoCmd.gen module' (LocksCollection locks) typesCache
                Assert.Equal(expectedOutput.Trim(), outputText.Trim()))
            |> Result.mapError (fun error -> failwithf "%A" error)))
    |> Result.mapError (fun error -> failwithf "%A" error)
