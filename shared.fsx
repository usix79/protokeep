#r "nuget: FsToolkit.ErrorHandling"

open System
open System.Diagnostics
open System.IO
open FsToolkit.ErrorHandling

let inline (^) f x = f x

let resolveTarget () =
    if fsi.CommandLineArgs.Length > 1 then
        fsi.CommandLineArgs.[1].ToUpperInvariant()
    else
        "BUILD"

let resolveTargetArg () =
    if fsi.CommandLineArgs.Length > 2 then
        fsi.CommandLineArgs.[2].ToUpperInvariant()
    else
        ""

let exec cmd args workDir =

    let cmdTxt = $"EXEC {cmd} {args} from {workDir}"
    Console.WriteLine cmdTxt

    let startInfo = ProcessStartInfo(cmd, args)
    startInfo.WorkingDirectory <- workDir
    startInfo.UseShellExecute <- false
    startInfo.CreateNoWindow <- true

    use proc = new Process()
    proc.StartInfo <- startInfo

    proc.Start() |> ignore
    proc.WaitForExit()

    match proc.ExitCode with
    | 0 -> Ok()
    | x -> Error ^ $"{cmdTxt} exit with code {x}"

let dotnet = exec "dotnet"

let protokeep args workDir =
    exec "dotnet" $"protokeep {args}" workDir

let make title (targets: List<string * (string -> Result<unit, string>)>) =

    let target = resolveTarget ()
    let targetArg = resolveTargetArg ()
    Console.WriteLine $"{title} Make {target}"

    let targets =
        targets
        |> List.map (fun (name, action) -> name.ToUpperInvariant(), action)
        |> Map.ofList

    match targets.TryFind target with
    | Some action ->
        match action (targetArg) with
        | Ok() -> Console.WriteLine $"{title} Make {target} OK"
        | Error err -> Console.WriteLine $"{title} Make {target} ERROR {err}"
    | None -> Console.WriteLine $"{title} Make {target} ERROR Unknown Target"

let rm fileName dirName =
    let path = Path.Combine(dirName, fileName)

    if File.Exists path then
        File.Delete path

    Ok()

let rmAll dirName mask =
    result {
        if Directory.Exists dirName then
            for fileName in Directory.GetFiles(dirName, mask) do
                do! rm fileName dirName
        else
            return! Error ^ $"Directory {dirName} not found"
    }
