[<RequireQualifiedAccess>]
module rec Protogen.FsharpConvertersCmd

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
            let fileContent = gen module' locks typesCache
            let fileName =
                if Path.GetExtension(outputFileName) <> ".fs" then  outputFileName + ".g.fs" else outputFileName
            Console.WriteLine($"Writing fsharp conveters to {fileName}")
            File.WriteAllText(fileName, fileContent)
            Ok () )
    | x -> Error $"expected arguments [-o|--output] outputFile, but {x}"

let Instance = {
    Name = "fsharp-converters"
    Description = "generate converters between protobuf classes and fsharp types: fsharp-converters [-o|--output] outputFile"
    Run = Handler
}

let gen (module':Module) (locks:LocksCollection) (typesCache:Types.TypesCache) =

    let txt = StringBuilder()

    let fromProtobuf fullNameTxt =
        line txt $"    static member FromProtobuf (x:ProtoClasses.{fullNameTxt}) : {fullNameTxt} ="

    let toProtobuf fullNameTxt =
        line txt $"    static member ToProtobuf (x:{fullNameTxt}) : ProtoClasses.{fullNameTxt} ="

    let convertionFrom type' = fieldFromProtobuf type' |> Option.map ((+) " |> ") |> Option.defaultValue ""

    let convertionTo type' = fieldToProtobuf type' |> Option.map ((+) " |> ") |> Option.defaultValue ""

    let ns = module'.Name

    let rec genItem = function
    | Enum info ->
        let fullNameTxt = info.Name |> dottedName
        fromProtobuf fullNameTxt
        line txt $"        enum<{fullNameTxt}>(int x)"
        toProtobuf fullNameTxt
        line txt $"        enum<ProtoClasses.{fullNameTxt}>(int x)"
    | Record info ->
        let fullNameTxt = info.Name |> dottedName
        let fullNameOfProtoClass = "ProtoClasses." + fullNameTxt

        fromProtobuf fullNameTxt
        line txt "        {"
        for fieldInfo in info.Fields do
            line txt $"            {fieldInfo.Name} = {genFieldFromProtobuf fullNameOfProtoClass fieldInfo}"
        line txt "        }"

        toProtobuf fullNameTxt
        line txt $"        let y = ProtoClasses.{fullNameTxt}()"
        for fieldInfo in info.Fields do
            genFieldToProtobuf $"x.{fieldInfo.Name}" $"y.{fieldInfo.Name}" fieldInfo
        line txt $"        y"
    | Union union ->
        fromProtobuf (dottedName union.Name)
        line txt "        match x.UnionCase with"
        for case in union.Cases do
            let rightValue =
                match case with
                | Types.EmptyRecord -> $"{dottedName case.Name}"
                | Types.SingleFieldRecord fieldInfo -> $"{dottedName case.Name}(x.{firstName case.Name}{convertionFrom fieldInfo.Type})"
                | Types.MultiFieldsRecord  -> $"x.{firstName case.Name} |> Convert{solidName ns}.FromProtobuf"
            line txt $"        | ProtoClasses.{dottedName union.Name}.UnionOneofCase.{firstName case.Name} -> {rightValue}"
        line txt $"        | _ -> {dottedName union.Name}.Unknown"

        toProtobuf (dottedName union.Name)
        line txt $"        let y = ProtoClasses.{dottedName union.Name}()"
        line txt "        match x with"
        for case in union.Cases do
            let values = case.Fields |> List.map Utils.getName |> String.concat ","
            let condition =
                $"        | {dottedName case.Name}" + if values <> "" then $" ({values})" else ""

            match case with
            | Types.EmptyRecord ->
                line txt $"{condition} -> y.{firstName case.Name} <- true"
            | Types.SingleFieldRecord fieldInfo ->
                line txt $"{condition} ->"
                genFieldToProtobuf fieldInfo.Name $"    y.{firstName case.Name}" fieldInfo
            | Types.MultiFieldsRecord ->
                line txt ($"{condition} -> y.{firstName case.Name} <- " +
                    $"Convert{solidName ns}.{firstName union.Name}Case{firstName case.Name}ToProtobuf({values})")

        line txt $"        | {dottedName union.Name}.Unknown -> ()"
        line txt "        y"

        for case in union.Cases do
            let fullCaseNameTxt = $"ProtoClasses.{dottedName union.Name}__{firstName case.Name}"
            let fieldsNames = case.Fields |> List.map(fun field -> field.Name) |> String.concat ","

            match case with
            | Types.MultiFieldsRecord ->
                line txt $"    static member FromProtobuf (x:{fullCaseNameTxt})  ="
                let values =
                    case.Fields
                    |> List.map(fun fieldInfo -> $"({genFieldFromProtobuf fullCaseNameTxt fieldInfo})")
                    |> String.concat ","

                line txt $"        {case.Name |> dottedName}"
                line txt $"            ({values})"

                line txt $"    static member {firstName union.Name}Case{firstName case.Name}ToProtobuf ({fieldsNames}) : {fullCaseNameTxt} ="
                line txt $"        let y = {fullCaseNameTxt}()"
                for fieldInfo in case.Fields do
                    genFieldToProtobuf $"{fieldInfo.Name}" $"y.{firstCharToUpper fieldInfo.Name}" fieldInfo
                line txt $"        y"
            | _ -> ()

    and genFieldFromProtobuf fullNameTxt fieldInfo =
        match fieldInfo.Type with
        | Optional t ->
            let caseValue = $"{fullNameTxt}.{fieldInfo.Name}OneofCase.{fieldInfo.Name}Value"
            $"if x.{firstCharToUpper fieldInfo.Name}Case = {caseValue} then Some (x.{fieldInfo.Name}Value{convertionFrom t}) else None"
        | t -> $"x.{firstCharToUpper fieldInfo.Name}{convertionFrom t}"

    and genFieldToProtobuf xName yName fieldInfo =
        match fieldInfo.Type with
        | Optional t ->
            line txt $"        match {xName} with"
            line txt $"        | Some v -> {yName}Value <- v{convertionTo t}"
            line txt $"        | None -> ()"
        | Array t | List t ->
            match fieldToProtobuf t with
            | Some cnv -> line txt $"        {yName}.AddRange({xName} |> Seq.map({cnv}))"
            | None -> line txt $"        {yName}.AddRange({xName})"
        | Map t ->
            match fieldToProtobuf t with
            | Some cnv ->
                line txt $"    for pair in {xName} do"
                line txt $"        {yName}.[pair.Key] <- pair.Value{cnv}"
            | None -> line txt $"        {yName}.Add({xName})"
        | t ->
            line txt $"        {yName} <- {xName}{convertionTo t}"

    line txt $"namespace Protogen.FsharpConverters"
    line txt $"type Convert{solidName module'.Name} () ="
    for item in module'.Items do
        genItem item

    txt.ToString()

let rec fieldFromProtobuf type' =
    match type' with
    | Bool
    | String
    | Int
    | Long
    | Float
    | Double -> None
    | Decimal scale -> Some $"fun v -> (decimal v) / {10. ** float(scale)}m"
    | Bytes -> Some "fun v -> v.ToByteArray()"
    | Timestamp -> Some "fun v -> v.ToDateTimeOffset()"
    | Duration -> Some "fun v -> v.ToTimeSpan()"
    | Guid  -> Some "fun v -> System.Guid(v.ToByteArray())"
    | Optional t -> failwith "direct convertion is not possible"
    | Array t ->
        match fieldFromProtobuf t with
        | Some convertion -> $"Seq.map({convertion}) |> Array.ofSeq"
        | None -> "Array.ofSeq"
        |> Some
    | List t ->
        match fieldFromProtobuf t with
        | Some convertion -> $"Seq.map({convertion}) |> List.ofSeq"
        | None -> "List.ofSeq"
        |> Some
    | Map t ->
        match fieldFromProtobuf t with
        | Some convertion -> $"x.Props |> Seq.map(fun pair -> pair.Key,pair.Value |> {convertion}) |> Map.ofSeq"
        | None -> "Seq.map(fun pair -> pair.Key,pair.Value) |> Map.ofSeq"
        |> Some
    | Complex typeName -> Some $"Convert{lastNames typeName |> solidName}.FromProtobuf"

let rec fieldToProtobuf type' =
    match type' with
    | Bool
    | String
    | Int
    | Long
    | Float
    | Double -> None
    | Decimal scale -> Some $"fun v -> int64(v * {10. ** float(scale)}m)"
    | Bytes -> Some "Google.Protobuf.ByteString.CopyFrom"
    | Timestamp -> Some "Google.Protobuf.WellKnownTypes.Timestamp.FromDateTimeOffset"
    | Duration -> Some "Google.Protobuf.WellKnownTypes.Duration.FromTimeSpan"
    | Guid -> Some "fun v -> Google.Protobuf.ByteString.CopyFrom(v.ToByteArray())"
    | Optional _ -> failwith "direct convertion is not possible"
    | Array _ | List _ -> failwith "direct convertion is supported, use AddRange"
    | Map _ -> failwith "direct convertion is supported, use Add"
    | Complex typeName -> Some $"Convert{lastNames typeName |> solidName}.ToProtobuf"