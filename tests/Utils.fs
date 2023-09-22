module Utils

open System.IO
open System.Runtime.CompilerServices

open Xunit.Sdk

type TestCasesFromFilesAttribute(testName: string, suffixes: string array) =
    inherit DataAttribute()

    static member GetProjectDirectory([<CallerFilePath>] ?currentFilePath: string) =
        match Path.GetDirectoryName(currentFilePath |> Option.defaultValue "") with
        | null -> "."
        | path -> path

    override x.GetData(testMethod: System.Reflection.MethodInfo) =
        let projectDir = TestCasesFromFilesAttribute.GetProjectDirectory()
        let casesDir = Path.Combine(projectDir, "cases")
        // get all file names, started from testName
        Directory.GetFiles(casesDir, $"{testName}.*.txt")
        |> Array.map Path.GetFileName
        |> Array.groupBy (fun name -> name.Split('.')[1]) // group by scenario name
        |> Array.map (fun (scenarioName, fileNames) ->
            [| scenarioName
               let fileSuffixes = fileNames |> Array.map (fun name -> name.Split('.')[2])

               yield!
                   suffixes
                   |> Array.map (fun suffix ->
                       match fileSuffixes |> Array.tryFindIndex ((=) suffix) with
                       | Some index -> File.ReadAllText(Path.Combine(casesDir, fileNames[index]))
                       | None -> $"File not found: '{testName}.{scenarioName}.{suffix}.txt'") |]
            |> Array.map (fun x -> x :> obj))
        |> Seq.ofArray
