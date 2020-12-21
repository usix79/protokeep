open System
open System.IO
open Protogen.Types

let commands: Command list = [
    Protogen.CheckCmd.Instance
    Protogen.LockCmd.Instance
    Protogen.ProtoCmd.Instance
    Protogen.FsharpTypesCmd.Instance
    Protogen.FsharpConvertersCmd.Instance
    Protogen.FableConvertersCmd.Instance
]

let processImports rootPath (module':Module) : Result<Module*TypesCache,string> =
    let prosessedFiles = Collections.Generic.HashSet<string>()

    let rec loadAllModules (module':Module) : Result<Module list,string list> =
        module'.Imports
        |> Protogen.Utils.traverse (fun fileName ->
            let fileName = Path.Combine(rootPath, fileName)
            if File.Exists fileName then Ok fileName
            else if File.Exists (fileName + ".pgen") then Ok (fileName + ".pgen")
            else Error [$"Import file not found: {fileName}"] )
        |> Result.bind (fun pgenFileNames ->
            pgenFileNames
            |> List.filter (prosessedFiles.Contains >> not)
            |> Protogen.Utils.traverse (fun fileName ->
                prosessedFiles.Add fileName |> ignore
                File.ReadAllText fileName
                |> Protogen.Parsers.parsePgenDoc
                |> Result.mapError List.singleton) )
        |> Result.bind (fun newModules ->
            newModules
            |> Protogen.Utils.traverse loadAllModules
            |> Result.map (List.concat >> ((@) newModules)) )

    let res =
        loadAllModules module'
        |> Result.bind (fun imports ->
            Types.resolveReferences module' imports
            |> Result.mapError (fun err -> err |> List.map (sprintf "%A")))
        |> Result.mapError (String.concat "\n")

    res

[<EntryPoint>]
let main argv =
    match argv |> List.ofArray with
    | ["--version"] | ["-V"] ->
        Console.WriteLine($"Protogen tool, version {Reflection.Assembly.GetExecutingAssembly().GetName().Version}")
        0
    | [] | ["--help"] | ["-H"] ->
        let dsc = """
Protogen tool
    Usage:
        protogen --help | -H
        protogen --version | -V
        protogen <pgenFileName> COMMAND ARGUMENTS
    Commands:
"""
        Console.Write dsc
        commands |> List.iter(fun cmd -> Console.WriteLine($"        {cmd.Name} - {cmd.Description}"))
        0
    | pgenFileName :: commandName :: args ->
        let command = commands |> List.tryFind (fun cmd -> cmd.Name = commandName)
        let res =
            match command with
            | Some cmd -> Ok cmd
            | None -> Error "Command not found"
            |> Result.bind(fun cmd ->
                if File.Exists pgenFileName then Ok pgenFileName
                else if File.Exists (pgenFileName + ".pgen") then Ok (pgenFileName + ".pgen")
                else Error $"File not found: {pgenFileName}"
                |> Result.bind(fun pgenFileName ->
                    File.ReadAllText pgenFileName
                    |> Protogen.Parsers.parsePgenDoc
                    |> Result.bind(fun module' ->
                        processImports (Path.GetDirectoryName pgenFileName) module'
                        |> Result.bind (fun (module',typesCache) ->
                            let lockFileName = pgenFileName + ".lock"
                            if File.Exists lockFileName then
                                File.ReadAllText lockFileName
                                |> Protogen.Parsers.parseLockDoc
                            else
                                Console.WriteLine "Warning, lock file not found, procceed with empty lock"
                                Ok []
                            |> Result.bind(fun locks ->
                                let args = if cmd.Name = "lock" then lockFileName::args else args // special case for lock cmd
                                cmd.Run module' (LocksCollection(locks)) typesCache args ))
                    )
            ))
        match res with
        | Ok _ -> 0
        | Error txt ->
            Console.Error.WriteLine ($"{commandName.ToUpper()} ERROR: {txt}")
            1

    | _ ->
        Console.WriteLine($"ERROR: invalid usage, `run protogen --help` for getting propper usage")
        1