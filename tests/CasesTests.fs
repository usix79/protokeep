module CasesTests

open System.IO
open System.Runtime.CompilerServices
open Xunit
open Xunit.Sdk
open Protokeep
open Protokeep.Types

type TestCasesFromFilesAttribute(dir: string) =
    inherit DataAttribute()

    static member GetProjectDirectory([<CallerFilePath>] ?currentFilePath: string) =
        match Path.GetDirectoryName(currentFilePath |> Option.defaultValue "") with
        | null -> "."
        | path -> path

    override x.GetData(testMethod: System.Reflection.MethodInfo) =
        let projectDir = TestCasesFromFilesAttribute.GetProjectDirectory()
        let casesDir = Path.Combine(projectDir, dir)

        Directory.GetFiles(casesDir, $"*.case")
        |> Array.map Path.GetFileName
        |> Array.map (fun (caseFileName) ->
            let caseName = caseFileName.Split('.')[0]

            let fileNames = Directory.GetFiles(casesDir, $"{caseName}.*")

            let commands =
                fileNames |> Array.map (fun (fileName: string) -> fileName.Split('.')[1])

            let docs =
                fileNames
                |> Array.map (fun fileName -> File.ReadAllText(Path.Combine(casesDir, fileName)))

            let map = Array.zip commands docs |> Map.ofArray
            [| caseName :> obj; map :> obj |])
        |> Seq.ofArray


[<Theory; TestCasesFromFiles("cases")>]
let testAllCases (caseName, docs: Map<string, string>) =
    match docs.TryFind "schema" with
    | Some schema ->
        match Parsers.parsePkDoc schema with
        | Ok module' ->
            match Types.resolveReferences module' [] with
            | Ok(module', typesCache) ->
                match Types.lock module' (LocksCollection []) typesCache with
                | Ok locks ->
                    let docs = docs |> Map.remove "schema"
                    let genNamespace = "Test.Converters"

                    for pair in docs do
                        let gen =
                            match pair.Key with
                            | "proto" -> ProtoCmd.gen
                            | "fsharp-types" -> FsharpTypesCmd.gen
                            | "fsharp-proto" -> FsharpProtoCmd.gen genNamespace
                            | "fsharp-json" -> FsharpJsonCmd.gen genNamespace
                            | "fsharp-fable" -> FsharpFableCmd.gen genNamespace
                            | "fsharp-mongo" -> FsharpMongoCmd.gen genNamespace
                            | cmd -> failwithf "Unknown command '%s'" cmd

                        let outputText = gen module' (LocksCollection locks) typesCache
                        Assert.Equal(pair.Value.Trim(), outputText.Trim())

                | Error error -> failwithf "%A" error
            | Error error -> failwithf "%A" error
        | Error error -> failwithf "%A" error
    | None -> failwithf "Schema not found in case '%s'" caseName
