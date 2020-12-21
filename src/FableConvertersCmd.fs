[<RequireQualifiedAccess>]
module rec Protogen.FableConvertersCmd

open System
open System.Text
open System.IO
open Types
open Codegen

let Handler module' locks typesCache = function
    | "-o"::outputFileName::args
    | "--output"::outputFileName::args ->
        Program.checkLock module' locks typesCache
        |> Result.bind(fun _ ->
            let fileContent:string = gen module' locks typesCache
            let fileName =
                if Path.GetExtension(outputFileName) <> ".fs" then  outputFileName + ".g.fs" else outputFileName
            Console.WriteLine($"Writing fable converters types to {fileName}")
            File.WriteAllText(fileName, fileContent)

            Program.checkArgCore "Protogen.fs" args
            |> Option.iter (fun coreFileName ->
                let coreFileText =
                    if (File.Exists coreFileName) then File.ReadAllText(coreFileName) else ""

                let updatedCoreFileText = CoreFsharp.update coreFileText "FableConverterHelpers" helpersBody
                Console.WriteLine($"Writing fable converters helpers to {coreFileName}")
                File.WriteAllText (coreFileName, updatedCoreFileText)
            )
            Ok () )
    | x -> Error $"expected arguments [-o|--output] outputFile, but {x}"

let Instance = {
    Name = "fable-converters"
    Description = """
generate converters between protobuf's json and fsharp types for fable environment: fable-converters [-o|--output] outputFile [--update-commons | --update-commons-in commonsFile]
                !!! Do not forget to add package Fable.SimpleJson"""
    Run = Handler
}

let gen (module':Module) (locks:LocksCollection) (typesCache:Types.TypesCache) =

    let txt = StringBuilder()

    let ns = module'.Name

    let rec genItem = function
    | Enum info ->
        let fullNameTxt = info.Name |> dottedName
        line txt $"    static member Default{firstName info.Name} ="
        line txt $"        lazy {fullNameTxt}.Unknown"

        line txt $"    static member {firstName info.Name}FromString = function"
        for symbol in info.Symbols do
            line txt $"        | \"{firstName info.Name}{symbol}\" -> {fullNameTxt}.{symbol}"
        line txt $"        | _ -> {fullNameTxt}.Unknown"

        line txt $"    static member {firstName info.Name}ToString = function"
        for symbol in info.Symbols do
            line txt $"        | {fullNameTxt}.{symbol} -> \"{firstName info.Name}{symbol}\""
        line txt $"        | _ -> \"Unknown\""

    | Record info ->
        let fullNameTxt = info.Name |> dottedName
        let lockItems = locks.Message(info.Name).LockItems

        line txt $"    static member Default{firstName info.Name}: Lazy<{fullNameTxt}> ="
        line txt $"        lazy {{"
        lockItems |> Seq.iter (function
            | Field fieldLock ->  line txt $"            {fieldLock.Name} = {defValue false fieldLock.Type}"
            | OneOf (name,unionName, _) -> line txt $"            {name} = {dottedName unionName}.Unknown" )
        line txt $"        }}"

        line txt $"    static member {firstName info.Name}FromJson (json: Json): {fullNameTxt} ="
        readObject "v" lockItems

        line txt $"        {{"
        lockItems |> Seq.iter (function
            | Field fieldLock ->
                match fieldLock.Type with
                | Array _ -> line txt $"            {fieldLock.Name} = unbox v{fieldLock.Name}"
                | List _ -> line txt $"            {fieldLock.Name} = v{fieldLock.Name} |> List.ofSeq"
                | Map _ -> line txt $"            {fieldLock.Name} = v{fieldLock.Name} |> Map.ofSeq"
                | _ -> line txt $"            {fieldLock.Name} = v{fieldLock.Name}"
            | OneOf (name, _, _) -> line txt $"            {name} = v{name}" )
        line txt $"        }}"

        line txt $"    static member {firstName info.Name}ToJson (x: {fullNameTxt}) ="
        writeObject "x." lockItems

    | Union info ->
        for case in info.Cases do
            let fieldsNames = case.Fields |> List.map(fun field -> field.Name) |> String.concat ","
            let lockItems = locks.Message(case.Name).LockItems

            match lockItems with
            | Types.MultiParamCase ->
                let values =
                    lockItems
                    |> List.map Types.messageLockItemName
                    |> String.concat ","

                line txt $"    static member {firstName info.Name}Case{firstName case.Name}FromJson (json: Json) ="
                readObject "" lockItems

                let convertedValues =
                    lockItems
                    |> Seq.map (function
                        | Field fieldLock ->
                            match fieldLock.Type with
                            | Array _ -> $"unbox {fieldLock.Name}"
                            | List _ -> $"{fieldLock.Name} |> List.ofSeq"
                            | Map _ -> $"{fieldLock.Name} |> Map.ofSeq"
                            | _ ->  fieldLock.Name
                        | OneOf (name, _, _) -> name)
                    |> String.concat ","

                line txt $"        {case.Name |> dottedName} ({convertedValues})"

                line txt $"    static member {firstName info.Name}Case{firstName case.Name}ToJson ({fieldsNames}) ="
                writeObject "" lockItems
            | _ -> ()
    and readObject prefix lockItems =
        lockItems |> Seq.iter (function
            | Field fieldLock ->  line txt $"        let mutable {prefix}{fieldLock.Name} = {defValue true fieldLock.Type}"
            | OneOf (name,unionName, _) -> line txt $"        let mutable {prefix}{name} = {dottedName unionName}.Unknown" )
        line txt $"        getProps json"
        line txt $"        |> Seq.iter(fun pair ->"
        line txt $"            match pair.Key with"
        lockItems |> Seq.iter (function
            | Field fieldLock ->
                let vName = prefix + fieldLock.Name
                let suffix = match fieldLock.Type with Optional _ -> "Value" | _ -> ""
                line txt $"            | \"{firstCharToUpper fieldLock.Name}{suffix}\" -> pair.Value |> {unpackField locks vName fieldLock.Type}"
            | OneOf (name, unionName, cases) ->
                let vName = prefix + name
                for case in cases do
                    let caseMessageType = Types.mergeName unionName case.CaseName
                    let caseMessageLock = locks.Message(caseMessageType)
                    let rightValue =
                        match caseMessageLock.LockItems with
                        | Types.EmptyCase -> $"ifBool (fun v -> {vName} <- {dottedName unionName}.{case.CaseName})"
                        | Types.SingleParamCase fieldInfo ->
                            let rightOp = $" |> {dottedName caseMessageType}"
                            $"{unpackField' rightOp locks vName fieldInfo.Type}"
                        | Types.MultiParamCase  -> $"(fun v -> {vName} <- v |> Convert{lastNames unionName |> solidName}.{firstName unionName}Case{case.CaseName}FromJson)"
                    line txt $"            | \"{firstCharToUpper  name}{case.CaseName}\" -> pair.Value |> {rightValue}"
                )
        line txt $"            | _ -> () )"
    and writeObject prefix lockItems =
        line txt $"        ["
        lockItems |> Seq.iter (function
            | Field fieldLock ->
                let vName = $"{prefix}{fieldLock.Name}"
                match fieldLock.Type with
                | Optional t ->
                    let inner = "v"
                    line txt $"           match {vName} with"
                    line txt $"           | Some v -> \"{fieldLock.Name}Value\", {packField locks inner t}"
                    line txt $"           | None -> ()"
                | _ -> line txt $"           \"{firstCharToUpper fieldLock.Name}\", {packField locks vName fieldLock.Type}"
            | OneOf (name, unionName, cases) ->
                line txt $"           match {prefix}{name} with"
                for case in cases do
                    let caseMessageType = Types.mergeName unionName case.CaseName
                    let caseMessageLock = locks.Message(caseMessageType)

                    let values =
                        caseMessageLock.LockItems
                        |> List.map Types.messageLockItemName
                        |> String.concat ","

                    let condition =
                        $"           | {dottedName caseMessageType}"
                        + if values <> "" then $" ({values})" else ""

                    let jsonConstructor =
                        match caseMessageLock.LockItems with
                        | Types.EmptyCase -> "JBool (true)"
                        | Types.SingleParamCase fieldInfo -> $"{packField locks values fieldInfo.Type}"
                        | Types.MultiParamCase  -> $"Convert{lastNames unionName |> solidName}.{firstName unionName}Case{case.CaseName}ToJson ({values})"
                    line txt $"{condition} -> \"{firstCharToUpper name}{case.CaseName}\", {jsonConstructor}"

                line txt "           | _ -> ()" )
        line txt $"        ] |> Map.ofList |> JObject"


    line txt $"namespace Protogen.FableConverters"
    line txt $"open Fable.SimpleJson"
    line txt $"open Protogen.FableConverterHelpers"
    line txt $"type Convert{solidName module'.Name} () ="
    for item in module'.Items do
        genItem item

    txt.ToString()

let rec defValue isMutable = function
    | Bool -> "false"
    | String -> "\"\""
    | Int -> "0"
    | Long -> "0L"
    | Float -> "0.f"
    | Double -> "0."
    | Decimal _ -> "0m"
    | Bytes -> "Array.empty"
    | Timestamp -> "System.DateTimeOffset.MinValue"
    | Duration -> "System.TimeSpan.Zero"
    | Guid  -> "System.Guid.Empty"
    | Optional _ -> "None"
    | Array _ -> if isMutable then "ResizeArray()" else "Array.empty"
    | List _ -> if isMutable then "ResizeArray()" else "List.empty"
    | Map _ -> if isMutable then "ResizeArray()" else "Map.empty"
    | Complex typeName -> $"Convert{lastNames typeName |> solidName}.Default{firstName typeName}.Value"

let unpackField' rightOp (locks:LocksCollection) vName =
    let rec f leftOp rightOp = function
        | Bool -> $"ifBool (fun v -> {leftOp}v{rightOp})"
        | String -> $"ifString (fun v -> {leftOp}v{rightOp})"
        | Int | Long | Float | Double -> $"ifNumber (fun v -> {leftOp}v |> unbox{rightOp})"
        | Decimal scale -> $"ifNumber (fun v -> {leftOp}v / {10. ** float(scale)}. |> unbox{rightOp})"
        | Bytes -> $"ifString (fun v -> {leftOp}v |> System.Convert.FromBase64String{rightOp})"
        | Timestamp -> $"ifString (fun v -> {leftOp}v |> toDateTimeOffset{rightOp})"
        | Duration -> $"ifString (fun v -> {leftOp}v |> toTimeSpan{rightOp})"
        | Guid  -> $"ifString (fun v -> {leftOp}v |> System.Convert.FromBase64String |> System.Guid{rightOp})"
        | Optional t -> f $"{vName} <- " " |> Some" t
        | Array t | List t ->
            let inner = f "" $" |> {vName}.Add" t
            $"ifArray (Seq.iter ({inner}))"
        | Map t ->
            let inner = f "" $" |> fun v -> {vName}.Add(key, v)" t
            $"ifObject (Map.iter (fun key -> {inner}))"
        | Complex typeName ->
            if locks.IsEnum typeName then
                $"ifString (fun v -> {leftOp}v |> Convert{lastNames typeName |> solidName}.{firstName typeName}FromString{rightOp})"
            else
                $"ifObject (fun v -> {leftOp}v |> Convert{lastNames typeName |> solidName}.{firstName typeName}FromJson{rightOp})"

    f $"{vName} <- " rightOp

let unpackField = unpackField' ""

let packField (locks:LocksCollection) (vName:string) type' =
    let rec f vName = function
        | Bool -> $"JBool ({vName})"
        | String -> $"JString ({vName})"
        | Int | Long | Float | Double -> $"JNumber (unbox {vName})"
        | Decimal scale -> $"JNumber ({vName} * {10. ** float(scale)}m |> System.Decimal.Truncate |> unbox)"
        | Bytes -> $"JString ({vName} |> System.Convert.ToBase64String)"
        | Timestamp -> $"JString ({vName} |> fromDateTimeOffset)"
        | Duration -> $"JString ({vName} |> fromTimeSpan)"
        | Guid  -> $"JString ({vName}.ToByteArray() |> System.Convert.ToBase64String)"
        | Optional _ -> failwith "cannot upack optional field"
        | Array t | List t ->
            let inner = f "v" t
            $"JArray ({vName} |> Seq.map (fun v -> {inner}) |> List.ofSeq)"
        | Map t ->
            let inner = f "v" t
            $"JObject ({vName} |> Map.map (fun _ v -> {inner}))"
        | Complex typeName ->
            if locks.IsEnum typeName then
                $"JString ({vName} |> Convert{lastNames typeName |> solidName}.{firstName typeName}ToString)"
            else
                $"({vName} |> Convert{lastNames typeName |> solidName}.{firstName typeName}ToJson)"

    f vName type'



let helpersBody = """
    open Fable.SimpleJson

    let getProps = function JObject p -> p | _ -> Map.empty
    let ifBool action = function (JBool v) -> action v | _ -> ()
    let ifString action = function (JString v) -> action v | _ -> ()
    let ifNumber action = function (JNumber v) -> action v | _ -> ()
    let ifObject action = function (JObject v) -> action v | _ -> ()
    let ifArray action = function (JArray v) -> action v | _ -> ()

    let toDateTimeOffset (v:string) = System.DateTimeOffset.Parse v
    let fromDateTimeOffset (v:System.DateTimeOffset) = v.ToString("O")

    let durationRegex = System.Text.RegularExpressions.Regex @"^(-)?([0-9]{1,12})(\.[0-9]{1,9})?s$"
    let subsecondScalingFactors = [| 0; 100000000; 100000000; 10000000; 1000000; 100000; 10000; 1000; 100; 10; 1 |]
    let toTimeSpan (v:string) =
            let m = durationRegex.Match(v)
            match m.Success with
            | true ->
                let signText = m.Groups.[1].Value
                let secondsText = m.Groups.[2].Value
                let subseconds = m.Groups.[3].Value
                let sign = if signText = "-" then -1. else 1.

                let seconds = System.Int64.Parse(secondsText) |> float
                let milliseconds =
                    if subseconds <> "" then
                        let parsedFraction = System.Int32.Parse(subseconds.Substring(1))
                        parsedFraction * (subsecondScalingFactors.[subseconds.Length]) / 1000000 |> float
                    else 0.

                System.TimeSpan.FromMilliseconds(sign * (seconds * 1000. + milliseconds))
            | false -> failwithf "Invalid Duration value: %s"  v

    let fromTimeSpan (v:System.TimeSpan) =
        sprintf "%d.%ds" (int64 v.TotalSeconds) v.Milliseconds
"""