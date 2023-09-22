module FsharpTypesTests

open Xunit
open Protokeep.Types
open Protokeep

open Utils

[<Theory; TestCasesFromFiles("FsharpTypes", [| "Input"; "Output" |])>]
let testAllCases (scenarioName, input, expectedOutput: string) =
    Parsers.parsePkDoc input
    |> Result.bind (fun module' ->
        Types.resolveReferences module' []
        |> Result.mapError (fun error -> failwithf "%A" error)
        |> Result.bind (fun (module', typesCache) ->
            Types.lock module' (LocksCollection []) typesCache
            |> Result.map (fun locks ->
                let outputText = FsharpTypesCmd.gen module' (LocksCollection locks) typesCache
                Assert.Equal(expectedOutput.Trim(), outputText.Trim()))
            |> Result.mapError (fun error -> failwithf "%A" error)))
    |> Result.mapError (fun error -> failwithf "%A" error)
