[<RequireQualifiedAccess>]
module rec Protogen.FsharpConvertersCmd

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
            let fileContent = gen module' locks typesCache
            let fileName =
                if Path.GetExtension(outputFileName) <> ".fs" then  outputFileName + ".g.fs" else outputFileName
            Console.WriteLine($"Writing fsharp types to {fileName}")
            File.WriteAllText(fileName, fileContent)
            Ok () )
    | x -> Error $"expected arguments [-o|--output] outputFile, but {x}"

let Instance = {
    Name = "fsharp-converters"
    Description = "generate converters between protobuf classes and fsharp types: fsharp-converters [-o|--output] outputFile"
    Run = Handler
}

let gen (module':Module) (locks:LockItem list) (typesCache:Types.TypesCache) =
    let messageLockCache =
        locks |> List.choose(function MessageLock item -> Some(item.Name, item) | _ -> None) |> Map.ofList

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
        let fullNameTxt = Types.mergeName ns info.Name |> dottedName
        fromProtobuf fullNameTxt
        line txt $"        enum<{fullNameTxt}>(int x)"
        toProtobuf fullNameTxt
        line txt $"        enum<ProtoClasses.{fullNameTxt}>(int x)"
    | Record info ->
        let typeName = Types.mergeName ns info.Name
        let fullNameTxt = typeName |> dottedName
        let lockItems = messageLockCache.[typeName].LockItems

        fromProtobuf fullNameTxt
        line txt "        {"
        lockItems |> List.iter(fun lockItem ->
            line txt $"            {Types.messageLockItemName lockItem} = {genLockItemFromProtobuf fullNameTxt lockItem}")
        line txt "        }"

        toProtobuf fullNameTxt
        line txt $"        let y = ProtoClasses.{fullNameTxt}()"
        lockItems
        |> List.iter (fun item ->
            let itemName = Types.messageLockItemName item
            genLockItemToProtobuf $"x.{itemName}" $"y.{itemName}" item)
        line txt $"        y"
    | Union info ->
        let typeName = Types.mergeName ns info.Name
        let fullNameTxt = typeName |> dottedName
        for case in info.Cases do
            let caseTypeName = Types.mergeName typeName case.Name
            let fullCaseNameTxt = $"ProtoClasses.{fullNameTxt}__{case.Name}"
            let fieldsNames = case.Fields |> List.map(fun field -> field.Name) |> String.concat ","
            let lockItems = messageLockCache.[caseTypeName].LockItems

            line txt $"    static member FromProtobuf (x:{fullCaseNameTxt})  ="
            if lockItems.IsEmpty then
                line txt $"        {caseTypeName |> dottedName}"
            else
                let values =
                    lockItems
                    |> List.map(fun lockItem -> $"({genLockItemFromProtobuf fullNameTxt lockItem})")
                    |> String.concat ","

                line txt $"        {caseTypeName |> dottedName} ({values})"

            line txt $"    static member ToProtobuf{info.Name}Case{case.Name} ({fieldsNames}) : {fullCaseNameTxt} ="
            line txt $"        let y = {fullCaseNameTxt}()"
            lockItems |> List.iter (fun item ->
                let itemName = Types.messageLockItemName item
                genLockItemToProtobuf $"{itemName}" $"y.{firstCharToUpper itemName}" item)
            line txt $"        y"

    and genLockItemFromProtobuf fullNameTxt = function
        | Field item ->
            match item.Type with
            | Optional t ->
                let caseValue = $"ProtoClasses.{fullNameTxt}.{item.Name}OneofCase.{item.Name}Value"
                $"if x.{firstCharToUpper item.Name}Case = {caseValue} then Some (x.{item.Name}Value{convertionFrom t}) else None"
            | t -> $"x.{firstCharToUpper item.Name}{convertionFrom t}"
        | OneOf (name,_,cases) ->
            [ ""
              $"                match x.{name}Case with"
              for case in cases do
                $"                | ProtoClasses.{fullNameTxt}.{name}OneofCase.{name}{case.CaseName} -> x.{name}{case.CaseName} |> ConvertDomain.FromProtobuf"
              $"                | _ -> Domain.{name}.Unknown"
            ]
            |> String.concat "\n"

    and genLockItemToProtobuf xName yName = function
        | Field item ->
            match item.Type with
            | Optional t ->
                line txt $"        match {xName} with"
                line txt $"        | Some v -> {yName}Value <- v{convertionTo t}"
                line txt $"        | None -> ()"
            | Array t ->
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
        | OneOf (_,unionName,cases) ->
            line txt $"        match {xName} with"
            for case in cases do
                let values =
                    messageLockCache.[Types.mergeName unionName case.CaseName].LockItems
                    |> List.map Types.messageLockItemName
                    |> String.concat ","
                let condition =
                    $"        | {dottedName unionName}.{case.CaseName}"
                    + if values <> "" then $" ({values})" else ""
                line txt $"{condition} -> {yName}{case.CaseName} <- Convert{lastNames unionName |> solidName}.ToProtobuf{firstName unionName}Case{case.CaseName}({values})"
            line txt $"        | {dottedName unionName}.Unknown -> ()"

    line txt $"namespace ProtoConverters.FsharpTypes"
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
    | Array _ -> failwith "direct convertion is supported, use AddRange"
    | Map _ -> failwith "direct convertion is supported, use Add"
    | Complex typeName -> Some $"Convert{lastNames typeName |> solidName}.ToProtobuf"