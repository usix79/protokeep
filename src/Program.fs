open System
open System.IO
open Protokeep.Types

let commands: Command list =
    [ Protokeep.CheckCmd.Instance
      Protokeep.LockCmd.Instance
      Protokeep.ProtoCmd.Instance
      Protokeep.FsharpTypesCmd.Instance
      Protokeep.FsharpProtoConvertersCmd.Instance
      Protokeep.FsharpJsonConvertersCmd.Instance
      Protokeep.FableConvertersCmd.Instance ]

let processImports rootPath (module': Module) : Result<Module * TypesCache, string> =
    let prosessedFiles = Collections.Generic.HashSet<string>()

    let rec loadAllModules (module': Module) : Result<Module list, string list> =
        module'.Imports
        |> Protokeep.Common.traverse (fun fileName ->
            let fileName = Path.Combine(rootPath, fileName)

            if File.Exists fileName then
                Ok fileName
            else if File.Exists(fileName + ".protokeep") then
                Ok(fileName + ".protokeep")
            else
                Error [ $"Import file not found: {fileName}" ])
        |> Result.bind (fun pkFileNames ->
            pkFileNames
            |> List.filter (prosessedFiles.Contains >> not)
            |> Protokeep.Common.traverse (fun fileName ->
                prosessedFiles.Add fileName |> ignore

                File.ReadAllText fileName
                |> Protokeep.Parsers.parsePkDoc
                |> Result.mapError List.singleton))
        |> Result.bind (fun newModules ->
            newModules
            |> Protokeep.Common.traverse loadAllModules
            |> Result.map (List.concat >> ((@) newModules)))

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
    | [ "--version" ]
    | [ "-V" ] ->
        Console.WriteLine($"Protokeeper tool, version {Reflection.Assembly.GetExecutingAssembly().GetName().Version}")
        0
    | []
    | [ "--help" ]
    | [ "-H" ] ->
        let dsc =
            """
Protokeeper tool
    Usage:
        protokeep --help | -H
        protokeep --version | -V
        protokeep <ProtokeepFileName> COMMAND ARGUMENTS
    Commands:
"""

        Console.Write dsc

        commands
        |> List.iter (fun cmd -> Console.WriteLine($"        {cmd.Name} - {cmd.Description}"))

        0
    | pkFileName :: commandName :: args ->
        let command = commands |> List.tryFind (fun cmd -> cmd.Name = commandName)

        let res =
            match command with
            | Some cmd -> Ok cmd
            | None -> Error "Command not found"
            |> Result.bind (fun cmd ->
                if File.Exists pkFileName then
                    Ok pkFileName
                else if File.Exists(pkFileName + ".protokeep") then
                    Ok(pkFileName + ".protokeep")
                else
                    Error $"File not found: {pkFileName}"
                |> Result.bind (fun pkFileName ->
                    File.ReadAllText pkFileName
                    |> Protokeep.Parsers.parsePkDoc
                    |> Result.bind (fun module' ->
                        processImports (Path.GetDirectoryName pkFileName) module'
                        |> Result.bind (fun (module', typesCache) ->
                            let lockFileName = pkFileName + ".lock"

                            if File.Exists lockFileName then
                                File.ReadAllText lockFileName |> Protokeep.Parsers.parseLockDoc
                            else
                                Console.WriteLine "Warning, lock file not found, procceed with empty lock"
                                Ok []
                            |> Result.bind (fun locks ->
                                let args = if cmd.Name = "lock" then lockFileName :: args else args // special case for lock cmd
                                cmd.Run module' (LocksCollection(locks)) typesCache args)))))

        match res with
        | Ok _ -> 0
        | Error txt ->
            Console.Error.WriteLine($"{commandName.ToUpper()} ERROR: {txt}")
            1

    | _ ->
        Console.WriteLine($"ERROR: invalid usage, `run protokeepr --help` for getting propper usage")
        1
