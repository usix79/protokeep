module Protokeep.Infra

open System.IO
open System.Reflection
open Types

let checkLock module' locks typesCache =
    Types.lock module' locks typesCache
    |> Result.mapError (sprintf "When try to check current lock: %A")
    |> Result.bind (fun newlocks ->
        if locks.HasChanges newlocks then
            Error "Lock file is not corresponded to types definition. Run Protokeep lock first."
        else
            Ok())

let rec checkArgUpdateCommons defaultName =
    function
    | [] -> None
    | "--update-commons" :: _ -> Some defaultName
    | "--update-commons-in" :: fileName :: _ -> Some fileName
    | _ :: tail -> checkArgUpdateCommons defaultName tail

let rec checkArgNamespace =
    function
    | [] -> None
    | "--namespace" :: ns :: _
    | "-ns" :: ns :: _ -> Some ns
    | _ :: tail -> checkArgNamespace tail


let loadEmbeddedFile (name: string) =
    let asm = Assembly.GetExecutingAssembly()

    let resourceName =
        asm.GetManifestResourceNames() |> Array.find (fun n -> n.EndsWith(name))

    use stream = asm.GetManifestResourceStream(resourceName)
    use reader = new StreamReader(stream)
    reader.ReadToEnd()


let updateCommons (targetFileName: string) args (fn: string -> string) =

    let defaultCommonsFileName =
        Path.Combine(Path.GetDirectoryName(targetFileName), $"Protokeep.g.fs")

    checkArgUpdateCommons defaultCommonsFileName args
    |> Option.iter
       ^ fun commonFileName ->
           let content =
               match File.Exists commonFileName with
               | true -> File.ReadAllText(commonFileName)
               | false -> ""

           let updatedContent = fn content

           printfn $"Updating common file: {commonFileName}"
           File.WriteAllText(commonFileName, updatedContent)

    Ok()

let writeFile (fileName: string) (ext: string) (content: string) =
    let fileName =
        if Path.GetExtension(fileName) <> ext then
            fileName + ext
        else
            fileName

    printfn $"Rewriting {fileName}"
    File.WriteAllText(fileName, content)

    fileName
