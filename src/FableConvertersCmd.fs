[<RequireQualifiedAccess>]
module rec Protogen.FableConvertersCmd

open System
open System.Text
open System.IO
open Types
open Codegen

let Handler module' locks = function
    | "-o"::outputFileName::args
    | "--output"::outputFileName::args ->
        checkLock module' locks
        |> Result.bind(fun typesCache ->
            let fileContent:string = gen module' locks typesCache
            let fileName =
                if Path.GetExtension(outputFileName) <> ".fs" then  outputFileName + ".g.fs" else outputFileName
            Console.WriteLine($"Writing fable converters types to {fileName}")
            let fileContent = fileContent.Replace ("open Helpers", helpersBody)
            File.WriteAllText(fileName, fileContent)
            Ok () )
    | x -> Error $"expected arguments [-o|--output] outputFile, but {x}"

let Instance = {
    Name = "fable-converters"
    Description = """
generate converters between protobuf's json and fsharp types for fable environment: fable-converters [-o|--output] outputFile
                !!! Do not forget to add package Fable.SimpleJson"""
    Run = Handler
}

let gen (module':Module) (locks:LockItem list) (typesCache:Types.TypesCache) =
    let enumLocksCache =
        locks |> List.choose(function EnumLock item -> Some(item.Name, item) | _ -> None) |> Map.ofList
    let messageLockCache =
        locks |> List.choose(function MessageLock item -> Some(item.Name, item) | _ -> None) |> Map.ofList

    let txt = StringBuilder()

    let ns = module'.Name

    let rec genItem = function
    | Enum info ->
        let fullNameTxt = Types.mergeName ns info.Name |> dottedName
        line txt $"    static member Default{info.Name} ="
        line txt $"        lazy {fullNameTxt}.Unknown"

        line txt $"    static member {info.Name}FromString = function"
        for symbol in info.Symbols do
            line txt $"        | \"{symbol}\" -> {fullNameTxt}.{symbol}"
        line txt $"        | _ -> {fullNameTxt}.Unknown"

        line txt $"    static member {info.Name}ToString = function"
        for symbol in info.Symbols do
            line txt $"        | {fullNameTxt}.{symbol} -> \"{symbol}\""
        line txt $"        | _ -> \"Unknown\""

    | Record info ->
        let typeName = Types.mergeName ns info.Name
        let fullNameTxt = typeName |> dottedName
        let lockItems = messageLockCache.[typeName].LockItems

        line txt $"    static member Default{info.Name}: Lazy<{fullNameTxt}> ="
        line txt $"        lazy {{"
        lockItems |> Seq.iter (function
            | Field fieldLock ->  line txt $"            {fieldLock.Name} = {defValue false fieldLock.Type}"
            | OneOf (name,unionName, _) -> line txt $"            {name} = {dottedName unionName}.Unknown" )
        line txt $"        }}"

        line txt $"    static member {info.Name}FromJson (json: Json): {fullNameTxt} ="
        readObject "v" lockItems

        line txt $"        {{"
        lockItems |> Seq.iter (function
            | Field fieldLock ->
                match fieldLock.Type with
                | Array _ -> line txt $"            {fieldLock.Name} = unbox v{fieldLock.Name}"
                | Map _ -> line txt $"            {fieldLock.Name} = v{fieldLock.Name} |> Map.ofSeq"
                | _ -> line txt $"            {fieldLock.Name} = v{fieldLock.Name}"
            | OneOf (name, _, _) -> line txt $"            {name} = v{name}" )
        line txt $"        }}"

        line txt $"    static member {info.Name}ToJson (x: {fullNameTxt}) ="
        writeObject "x." lockItems

    | Union info ->
        let typeName = Types.mergeName ns info.Name
        for case in info.Cases do
            let caseTypeName = Types.mergeName typeName case.Name
            let fieldsNames = case.Fields |> List.map(fun field -> field.Name) |> String.concat ","
            let lockItems = messageLockCache.[caseTypeName].LockItems

            match lockItems with
            | Types.MultiParamCase ->
                let values =
                    lockItems
                    |> List.map Types.messageLockItemName
                    |> String.concat ","

                line txt $"    static member {info.Name}Case{case.Name}FromJson (json: Json) ="
                readObject "" lockItems

                let convertedValues =
                    lockItems
                    |> Seq.map (function
                        | Field fieldLock ->
                            match fieldLock.Type with
                            | Array _ -> $"unbox {fieldLock.Name}"
                            | Map _ -> $"{fieldLock.Name} |> Map.ofSeq"
                            | _ ->  fieldLock.Name
                        | OneOf (name, _, _) -> name)
                    |> String.concat ","

                line txt $"        {caseTypeName |> dottedName} ({convertedValues})"

                line txt $"    static member {info.Name}Case{case.Name}ToJson ({fieldsNames}) ="
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
                line txt $"            | \"{firstCharToUpper fieldLock.Name}{suffix}\" -> pair.Value |> {unpackField enumLocksCache vName fieldLock.Type}"
            | OneOf (name, unionName, cases) ->
                let vName = prefix + name
                for case in cases do
                    let caseMessageType = Types.mergeName unionName case.CaseName
                    let caseMessageLock = messageLockCache.[caseMessageType]
                    let rightValue =
                        match caseMessageLock.LockItems with
                        | Types.EmptyCase -> $"ifBool (fun v -> {vName} <- {dottedName unionName}.{case.CaseName})"
                        | Types.SingleParamCase fieldInfo ->
                            let rightOp = $" |> {dottedName caseMessageType}"
                            $"{unpackField' rightOp enumLocksCache vName fieldInfo.Type}"
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
                    line txt $"           | Some v -> \"{fieldLock.Name}Value\", {packField enumLocksCache inner t}"
                    line txt $"           | None -> ()"
                | _ -> line txt $"           \"{firstCharToUpper fieldLock.Name}\", {packField enumLocksCache vName fieldLock.Type}"
            | OneOf (name, unionName, cases) ->
                line txt $"           match {prefix}{name} with"
                for case in cases do
                    let caseMessageType = Types.mergeName unionName case.CaseName
                    let caseMessageLock = messageLockCache.[caseMessageType]

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
                        | Types.SingleParamCase fieldInfo -> $"{packField enumLocksCache values fieldInfo.Type}"
                        | Types.MultiParamCase  -> $"Convert{lastNames unionName |> solidName}.{firstName unionName}Case{case.CaseName}ToJson ({values})"
                    line txt $"{condition} -> \"{firstCharToUpper name}{case.CaseName}\", {jsonConstructor}"

                line txt "           | _ -> ()" )
        line txt $"        ] |> Map.ofList |> JObject"


    line txt $"namespace ProtoConverters.Fable"
    line txt $"open Fable.SimpleJson"
    line txt $"open Helpers"
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
    | Map _ -> if isMutable then "ResizeArray()" else "Map.empty"
    | Complex typeName -> $"Convert{lastNames typeName |> solidName}.Default{firstName typeName}.Value"

let unpackField' rightOp (enumLockCache:Map<ComplexName,EnumLock>) vName =
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
        | Array t ->
            let inner = f "" $" |> {vName}.Add" t
            $"ifArray (Seq.iter ({inner}))"
        | Map t ->
            let inner = f "" $" |> fun v -> {vName}.Add(key, v)" t
            $"ifObject (Map.iter (fun key -> {inner}))"
        | Complex typeName ->
            if enumLockCache.ContainsKey typeName then
                $"ifString (fun v -> {leftOp}v |> Convert{lastNames typeName |> solidName}.{firstName typeName}FromString{rightOp})"
            else
                $"ifObject (fun v -> {leftOp}v |> Convert{lastNames typeName |> solidName}.{firstName typeName}FromJson{rightOp})"

    f $"{vName} <- " rightOp

let unpackField = unpackField' ""

let packField (enumLockCache:Map<ComplexName,EnumLock>) (vName:string) type' =
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
        | Array t ->
            let inner = f "v" t
            $"JArray ({vName} |> Array.map (fun v -> {inner}) |> List.ofSeq)"
        | Map t ->
            let inner = f "v" t
            $"JObject ({vName} |> Map.map (fun _ v -> {inner}))"
        | Complex typeName ->
            if enumLockCache.ContainsKey typeName then
                $"JString ({vName} |> Convert{lastNames typeName |> solidName}.{firstName typeName}ToString)"
            else
                $"({vName} |> Convert{lastNames typeName |> solidName}.{firstName typeName}ToJson)"

    f vName type'



let helpersBody = """
module Helpers =
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

open Helpers
"""