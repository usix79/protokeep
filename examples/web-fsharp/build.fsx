#r "paket:
nuget Fake.Core.Target prerelease
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem"
#load "./.fake/build.fsx/intellisense.fsx"

open System.IO
open Fake.Core
open Fake.DotNet
open Fake.IO


Target.initEnvironment ()

let protogenDirectory = "../../"
let protogenDll = "../../src/bin/Debug/net5.0/Protogen.dll"
let pgenFile = "domain.pgen"
let pgenFileSubdomain = "subdomain.pgen"
let protoClassesDir = "./ProtoClasses/"
let protoFile = protoClassesDir + "Domain.proto"
let protoFileSubdomain = protoClassesDir + "Domain.Subdomain.proto"
let fsharpTypesFile = "./server/Domain.fs"
let fsharpTypesFileSubdomain = "./server/Subdomain.fs"
let fsharpCommonsFile = "./server/Protogen.fs"
let fsharpConvertersFile = "./server/DomainConverters.fs"
let fsharpConvertersFileSubdomain = "./server/SubdomainConverters.fs"
let fsharpTypesFileClient = "./client/Domain.fs"
let fsharpTypesFileClientSubdomain = "./client/Subdomain.fs"
let fableConvertersFile = "./client/DomainConverters.fs"
let fableConvertersFileSubdomain = "./client/SubdomainConverters.fs"
let fableCommonsFile = "./client/Protogen.fs"

let pgenFileForBetting = "betting.pgen"
let protoFileForBetting = protoClassesDir + "Betting.proto"
let fsharpTypesFileForBetting = "./server/Betting.fs"
let pgenFileForBettingFootball = "betting-football.pgen"
let protoFileForBettingFootball = protoClassesDir + "BettingFootball.proto"
let fsharpTypesFileForBettingFootball = "./server/BettingFootball.fs"
let fsharpConvertersFileforBetting = "./server/BettingConverters.fs"
let fsharpConvertersFileforBettingFootball = "./server/BettingFootballConverters.fs"

let runTool cmd args workingDir =
    let arguments = args |> String.split ' ' |> Arguments.OfArgs
    RawCommand (cmd, arguments)
    |> CreateProcess.fromCommand
    |> CreateProcess.withWorkingDirectory workingDir
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore

let dotnetWithArgs (args:string list) cmd workingDir =
    let argsString = System.String.Join (" ", args)
    let result =
        DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd argsString
    if result.ExitCode <> 0 then failwithf "'dotnet %s %s' failed in %s" cmd argsString workingDir

let dotnet = dotnetWithArgs []

Target.create "Clean" (fun _ ->
    Shell.cleanDirs [protoClassesDir + "obj"; protoClassesDir + "bin"]
    Directory.GetFiles(protoClassesDir, "*.proto")
    |> File.deleteAll
)

Target.create "Gen" (fun _ ->
    dotnet "build" protogenDirectory
    dotnetWithArgs [pgenFile; "lock"] protogenDll __SOURCE_DIRECTORY__
    dotnetWithArgs [pgenFile; "proto"; "-o"; protoFile] protogenDll __SOURCE_DIRECTORY__
    dotnetWithArgs [pgenFile; "fsharp-types"; "-o"; fsharpTypesFile; "--update-commons-in"; fsharpCommonsFile] protogenDll __SOURCE_DIRECTORY__
    dotnetWithArgs [pgenFile; "fsharp-converters"; "-o"; fsharpConvertersFile; "--update-commons-in"; fsharpCommonsFile] protogenDll __SOURCE_DIRECTORY__
    dotnetWithArgs [pgenFile; "fsharp-types"; "-o"; fsharpTypesFileClient;  "--update-commons-in"; fableCommonsFile] protogenDll __SOURCE_DIRECTORY__
    dotnetWithArgs [pgenFile; "fable-converters"; "-o"; fableConvertersFile; "--update-commons-in"; fableCommonsFile] protogenDll __SOURCE_DIRECTORY__
    dotnetWithArgs [pgenFileSubdomain; "lock"] protogenDll __SOURCE_DIRECTORY__
    dotnetWithArgs [pgenFileSubdomain; "proto"; "-o"; protoFileSubdomain] protogenDll __SOURCE_DIRECTORY__
    dotnetWithArgs [pgenFileSubdomain; "fsharp-types"; "-o"; fsharpTypesFileSubdomain; "--update-commons-in"; fsharpCommonsFile] protogenDll __SOURCE_DIRECTORY__
    dotnetWithArgs [pgenFileSubdomain; "fsharp-converters"; "-o"; fsharpConvertersFileSubdomain; "--update-commons-in"; fsharpCommonsFile] protogenDll __SOURCE_DIRECTORY__
    dotnetWithArgs [pgenFileSubdomain; "fsharp-types"; "-o"; fsharpTypesFileClientSubdomain;  "--update-commons-in"; fableCommonsFile] protogenDll __SOURCE_DIRECTORY__
    dotnetWithArgs [pgenFileSubdomain; "fable-converters"; "-o"; fableConvertersFileSubdomain; "--update-commons-in"; fableCommonsFile] protogenDll __SOURCE_DIRECTORY__

    dotnetWithArgs [pgenFileForBetting; "lock"] protogenDll __SOURCE_DIRECTORY__
    dotnetWithArgs [pgenFileForBetting; "proto"; "-o"; protoFileForBetting] protogenDll __SOURCE_DIRECTORY__
    dotnetWithArgs [pgenFileForBetting; "fsharp-types"; "-o"; fsharpTypesFileForBetting] protogenDll __SOURCE_DIRECTORY__
    dotnetWithArgs [pgenFileForBetting; "fsharp-converters"; "-o"; fsharpConvertersFileforBetting; "--update-commons-in"; fsharpCommonsFile] protogenDll __SOURCE_DIRECTORY__
    dotnetWithArgs [pgenFileForBettingFootball; "lock"] protogenDll __SOURCE_DIRECTORY__
    dotnetWithArgs [pgenFileForBettingFootball; "proto"; "-o"; protoFileForBettingFootball] protogenDll __SOURCE_DIRECTORY__
    dotnetWithArgs [pgenFileForBettingFootball; "fsharp-types"; "-o"; fsharpTypesFileForBettingFootball] protogenDll __SOURCE_DIRECTORY__
    dotnetWithArgs [pgenFileForBettingFootball; "fsharp-converters"; "-o"; fsharpConvertersFileforBettingFootball; "--update-commons-in"; fsharpCommonsFile] protogenDll __SOURCE_DIRECTORY__
)

Target.create "Build" (fun _ ->
    dotnet "build" __SOURCE_DIRECTORY__
    dotnet "fable client" __SOURCE_DIRECTORY__
)

Target.create "Run" (fun _ ->
    [
        async {dotnet "watch --project ./server/ run" __SOURCE_DIRECTORY__}
        async {dotnet "fable watch client" __SOURCE_DIRECTORY__}
    ]
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore
)

open Fake.Core.TargetOperators

"Clean"
    ==> "Gen"
    ==> "Build"

Target.runOrDefaultWithArguments "Build"