[<RequireQualifiedAccess>]
module rec Protokeep.FableConvertersCmd

open System
open System.Text
open System.IO
open Types
open Codegen

let Handler module' locks typesCache =
    function
    | "-o" :: outputFileName :: args
    | "--output" :: outputFileName :: args ->
        Program.checkLock module' locks typesCache
        |> Result.bind (fun _ ->
            let fileContent: string = gen module' locks typesCache

            let fileName =
                if Path.GetExtension(outputFileName) <> ".fs" then
                    outputFileName + ".g.fs"
                else
                    outputFileName

            Console.WriteLine($"Writing fable converters types to {fileName}")
            File.WriteAllText(fileName, fileContent)

            let defaultCommonsFileName =
                Path.Combine(Path.GetDirectoryName(fileName), "Protokeep.fs")

            Program.checkArgUpdateCommons defaultCommonsFileName args
            |> Option.iter (fun coreFileName ->
                let coreFileText =
                    if (File.Exists coreFileName) then
                        File.ReadAllText(coreFileName)
                    else
                        ""

                let updatedCoreFileText =
                    CoreFsharp.update coreFileText "FableConverterHelpers" helpersBody

                Console.WriteLine($"Writing fable converters helpers to {coreFileName}")
                File.WriteAllText(coreFileName, updatedCoreFileText))

            Ok())
    | x -> Error $"expected arguments [-o|--output] outputFile, but {x}"

let Instance =
    { Name = "fable-converters"
      Description =
        """
generate converters between protobuf's json and fsharp types for fable environment: fable-converters [-o|--output] outputFile [--update-commons | --update-commons-in commonsFile]
                !!! Do not forget to add package Fable.SimpleJson"""
      Run = Handler }

let gen (module': Module) (locks: LocksCollection) (typesCache: Types.TypesCache) =

    let txt = StringBuilder()

    let ns = module'.Name

    let rec genItem =
        function
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

            line txt $"    static member Default{firstName info.Name}: Lazy<{fullNameTxt}> ="
            line txt $"        lazy {{"

            for fieldInfo in info.Fields do
                match fieldInfo.Type with
                | Types.IsUnion typesCache unionInfo ->
                    line txt $"            {fieldInfo.Name} = {dottedName unionInfo.Name}.Unknown"
                | _ -> line txt $"            {fieldInfo.Name} = {defValue false fieldInfo.Type}"

            line txt $"        }}"

            line txt $"    static member {firstName info.Name}FromJson (json: Json): {fullNameTxt} ="
            readObject "v" info

            line txt $"        {{"

            for fieldInfo in info.Fields do
                match fieldInfo.Type with
                | Types.IsUnion typesCache _ -> line txt $"            {fieldInfo.Name} = v{fieldInfo.Name}"
                | Array _ -> line txt $"            {fieldInfo.Name} = unbox v{fieldInfo.Name}"
                | List _ -> line txt $"            {fieldInfo.Name} = v{fieldInfo.Name} |> List.ofSeq"
                | Map _ -> line txt $"            {fieldInfo.Name} = v{fieldInfo.Name} |> Map.ofSeq"
                | _ -> line txt $"            {fieldInfo.Name} = v{fieldInfo.Name}"

            line txt $"        }}"

            line txt $"    static member {firstName info.Name}ToJson (x: {fullNameTxt}) ="
            writeObject "x." info

        | Union union ->

            line txt $"    static member {firstName union.Name}FromJson (json: Json): {dottedName union.Name} ="
            line txt $"        let mutable y = {dottedName union.Name}.Unknown"
            line txt $"        getProps json"
            line txt $"        |> Seq.iter(fun pair ->"
            line txt $"            match pair.Key with"

            for case in union.Cases do
                let rightValue =
                    match case with
                    | Types.EmptyRecord -> $"ifBool (fun v -> y <- {dottedName union.Name}.{firstName case.Name})"
                    | Types.SingleFieldRecord fieldInfo ->
                        unpackField' $" |> {dottedName case.Name}" typesCache "y" fieldInfo.Type
                    | Types.MultiFieldsRecord ->
                        $"(fun v -> y <- v |> Convert{solidName ns}.{firstName union.Name}Case{firstName case.Name}FromJson)"

                line txt $"            | \"{firstName case.Name}\" -> pair.Value |> {rightValue}"

            line txt $"            | _ -> () )"
            line txt $"        y"

            line txt $"    static member {firstName union.Name}ToJson (x:{dottedName union.Name}) ="
            line txt $"        match x with"

            for case in union.Cases do
                let values = case.Fields |> List.map Common.getName |> String.concat ","

                let condition =
                    $"        | {dottedName case.Name}"
                    + if values <> "" then $" ({values})" else ""

                let jsonConstructor =
                    match case with
                    | Types.EmptyRecord -> "JBool (true)"
                    | Types.SingleFieldRecord fieldInfo -> $"{packField typesCache values fieldInfo.Type}"
                    | Types.MultiFieldsRecord ->
                        $"Convert{lastNames union.Name |> solidName}.{firstName union.Name}Case{firstName case.Name}ToJson ({values})"

                line txt $"{condition} -> \"{firstName case.Name}\", {jsonConstructor}"

            line txt $"        | _ -> \"Unknown\", JBool (true)"
            line txt $"        |> List.singleton |> Map.ofList |> JObject"

            for case in union.Cases do
                let fieldsNames =
                    case.Fields |> List.map (fun field -> field.Name) |> String.concat ","

                match case with
                | Types.MultiFieldsRecord ->
                    line txt $"    static member {firstName union.Name}Case{firstName case.Name}FromJson (json: Json) ="
                    readObject "" case

                    let convertedValues =
                        case.Fields
                        |> Seq.map (fun fieldInfo ->
                            match fieldInfo.Type with
                            | Array _ -> $"unbox {fieldInfo.Name}"
                            | List _ -> $"{fieldInfo.Name} |> List.ofSeq"
                            | Map _ -> $"{fieldInfo.Name} |> Map.ofSeq"
                            | _ -> fieldInfo.Name)
                        |> String.concat ","

                    line txt $"        {case.Name |> dottedName} ({convertedValues})"

                    line
                        txt
                        $"    static member {firstName union.Name}Case{firstName case.Name}ToJson ({fieldsNames}) ="

                    writeObject "" case
                | _ -> ()

    and readObject prefix recordInfo =
        for fieldInfo in recordInfo.Fields do
            match fieldInfo.Type with
            | Types.IsUnion typesCache unionInfo ->
                line txt $"        let mutable {prefix}{fieldInfo.Name} = {dottedName unionInfo.Name}.Unknown"
            | _ -> line txt $"        let mutable {prefix}{fieldInfo.Name} = {defValue true fieldInfo.Type}"

        line txt $"        getProps json"
        line txt $"        |> Seq.iter(fun pair ->"
        line txt $"            match pair.Key with"

        for fieldInfo in recordInfo.Fields do
            let vName = prefix + fieldInfo.Name

            let suffix =
                match fieldInfo.Type with
                | Optional _ -> "Value"
                | _ -> ""

            line
                txt
                $"            | \"{firstCharToUpper fieldInfo.Name}{suffix}\" -> pair.Value |> {unpackField typesCache vName fieldInfo.Type}"

        line txt $"            | _ -> () )"

    and writeObject prefix recordInfo =
        line txt $"        ["

        for fieldInfo in recordInfo.Fields do
            let vName = $"{prefix}{fieldInfo.Name}"

            match fieldInfo.Type with
            | Optional t ->
                let inner = "v"
                line txt $"           match {vName} with"

                line
                    txt
                    $"           | Some v -> \"{firstCharToUpper fieldInfo.Name}Value\", {packField typesCache inner t}"

                line txt $"           | None -> ()"
            | _ ->
                line
                    txt
                    $"           \"{firstCharToUpper fieldInfo.Name}\", {packField typesCache vName fieldInfo.Type}"

        line txt $"        ] |> Map.ofList |> JObject"

    line txt $"namespace Protokeep.FableConverters"
    line txt $"open Fable.SimpleJson"
    line txt $"open Protokeep.FableConverterHelpers"
    line txt $"type Convert{solidName module'.Name} () ="

    for item in module'.Items do
        genItem item

    txt.ToString()

let rec defValue isMutable =
    function
    | Bool -> "false"
    | String -> "\"\""
    | Int -> "0"
    | Long -> "0L"
    | Float -> "0.f"
    | Double -> "0."
    | Decimal _ -> "0m"
    | Bytes -> "Array.empty"
    | Timestamp -> "System.DateTime.MinValue"
    | Duration -> "System.TimeSpan.Zero"
    | Guid -> "System.Guid.Empty"
    | Optional _ -> "None"
    | Array _ -> if isMutable then "ResizeArray()" else "Array.empty"
    | List _ -> if isMutable then "ResizeArray()" else "List.empty"
    | Map _ -> if isMutable then "ResizeArray()" else "Map.empty"
    | Complex typeName -> $"Convert{lastNames typeName |> solidName}.Default{firstName typeName}.Value"

let unpackField' rightOp (typesCache: Types.TypesCache) vName =
    let rec f leftOp rightOp =
        function
        | Bool -> $"ifBool (fun v -> {leftOp}v{rightOp})"
        | String -> $"ifString (fun v -> {leftOp}v{rightOp})"
        | Int
        | Long
        | Float
        | Double -> $"ifNumber (fun v -> {leftOp}v |> unbox{rightOp})"
        | Decimal scale -> $"ifNumber (fun v -> {leftOp}v / {10. ** float (scale)}. |> unbox{rightOp})"
        | Bytes -> $"ifString (fun v -> {leftOp}v |> System.Convert.FromBase64String{rightOp})"
        | Timestamp -> $"ifString (fun v -> {leftOp}v |> toDateTime{rightOp})"
        | Duration -> $"ifString (fun v -> {leftOp}v |> toTimeSpan{rightOp})"
        | Guid -> $"ifString (fun v -> {leftOp}v |> System.Convert.FromBase64String |> System.Guid{rightOp})"
        | Optional t -> f $"{vName} <- " " |> Some" t
        | Array t
        | List t ->
            let inner = f "" $" |> {vName}.Add" t
            $"ifArray (Seq.iter ({inner}))"
        | Map t ->
            let inner = f "" $" |> fun v -> {vName}.Add(key, v)" t
            $"ifObject (Map.iter (fun key -> {inner}))"
        | Complex typeName ->
            match typesCache.TryFind typeName with
            | Some(Enum _) ->
                $"ifString (fun v -> {leftOp}v |> Convert{lastNames typeName |> solidName}.{firstName typeName}FromString{rightOp})"
            | _ ->
                $"(fun v -> {leftOp}v |> Convert{lastNames typeName |> solidName}.{firstName typeName}FromJson{rightOp})"


    f $"{vName} <- " rightOp

let unpackField = unpackField' ""

let packField (typesCache: Types.TypesCache) (vName: string) type' =
    let rec f vName =
        function
        | Bool -> $"JBool ({vName})"
        | String -> $"JString ({vName})"
        | Int
        | Long
        | Float
        | Double -> $"JNumber (unbox {vName})"
        | Decimal scale -> $"JNumber ({vName} * {10. ** float (scale)}m |> System.Decimal.Truncate |> unbox)"
        | Bytes -> $"JString ({vName} |> System.Convert.ToBase64String)"
        | Timestamp -> $"JString ({vName} |> fromDateTime)"
        | Duration -> $"JString ({vName} |> fromTimeSpan)"
        | Guid -> $"JString ({vName}.ToByteArray() |> System.Convert.ToBase64String)"
        | Optional _ -> failwith "cannot upack optional field"
        | Array t
        | List t ->
            let inner = f "v" t
            $"JArray ({vName} |> Seq.map (fun v -> {inner}) |> List.ofSeq)"
        | Map t ->
            let inner = f "v" t
            $"JObject ({vName} |> Map.map (fun _ v -> {inner}))"
        | Complex typeName ->
            match typesCache.TryFind typeName with
            | Some(Enum _) ->
                $"JString ({vName} |> Convert{lastNames typeName |> solidName}.{firstName typeName}ToString)"
            | _ -> $"({vName} |> Convert{lastNames typeName |> solidName}.{firstName typeName}ToJson)"

    f vName type'



let helpersBody =
    """
    open Fable.SimpleJson

    let getProps = function JObject p -> p | _ -> Map.empty
    let ifBool action = function (JBool v) -> action v | _ -> ()
    let ifString action = function (JString v) -> action v | _ -> ()
    let ifNumber action = function (JNumber v) -> action v | _ -> ()
    let ifObject action = function (JObject v) -> action v | _ -> ()
    let ifArray action = function (JArray v) -> action v | _ -> ()

    let toDateTime (v:string) = System.DateTime.Parse v
    let fromDateTime (v:System.DateTime) = v.ToString("O")

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
