module FsharpProtoConvertersTests

open Xunit
open Protokeep.Types
open Protokeep

open Utils

[<Theory; TestCasesFromFiles("FsharpProtoConverters", [| "Input"; "Output" |])>]
let testAllCases (scenarioName, input, expectedOutput: string) =
    Parsers.parsePkDoc input
    |> Result.bind (fun module' ->
        Types.resolveReferences module' []
        |> Result.bind (fun (module', typesCache) ->
            Types.lock module' (LocksCollection []) typesCache
            |> Result.map (fun locks ->
                let outputText =
                    FsharpProtoConvertersCmd.gen module' (LocksCollection locks) typesCache

                Assert.Equal(expectedOutput.Trim(), outputText.Trim())))
        |> Result.mapError (fun error -> failwithf "%A" error))
    |> Result.mapError (fun error -> failwithf "%A" error)
