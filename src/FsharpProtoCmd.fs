[<RequireQualifiedAccess>]
module rec Protokeep.FsharpProtoCmd

open System.Text
open Types
open Codegen

let Handler module' locks typesCache =
    function
    | "-o" :: outputFileName :: args
    | "--output" :: outputFileName :: args ->
        Infra.checkLock module' locks typesCache
        |> Result.bind
           ^ fun _ ->
               let ns = Infra.checkArgNamespace args |> Option.defaultValue "Protokeep.FsharpProto"
               let _ = gen ns module' locks typesCache |> Infra.writeFile outputFileName ".fs"
               Ok()
    | x -> Error $"expected arguments [-o|--output] outputFile, but {x}"

let Instance =
    { Name = "fsharp-proto"
      Description = "generate converters between protobuf classes and fsharp types"
      Run = Handler }

let gen genNamespace (module': Module) (locks: LocksCollection) (typesCache: Types.TypesCache) =

    let ns = module'.Name

    let txt = StringBuilder()

    let fromProtobuf fullNameTxt =
        line txt $"    static member FromProtobuf(x: ProtoClasses.{fullNameTxt}) : {fullNameTxt} ="

    let toProtobuf fullNameTxt =
        line txt $"    static member ToProtobuf(x: {fullNameTxt}) : ProtoClasses.{fullNameTxt} ="

    let rec genItem =
        function
        | Enum info ->
            let fullNameTxt = info.Name |> dottedName
            fromProtobuf fullNameTxt
            line txt $"        LanguagePrimitives.EnumOfValue ({FsharpTypesCmd.primitiveTypeToString info.Type} x)"
            line txt ""
            toProtobuf fullNameTxt
            line txt $"        enum<ProtoClasses.{fullNameTxt}> (int x)"
            line txt ""
        | Record info ->
            let fullNameTxt = info.Name |> dottedName
            let fullNameOfProtoClass = "ProtoClasses." + fullNameTxt

            fromProtobuf fullNameTxt
            line txt "        {"

            for fieldInfo in info.Fields do
                line txt $"            {fieldInfo.Name} = {genFieldFromProtobuf fullNameOfProtoClass fieldInfo}"

            line txt "        }"
            line txt ""

            toProtobuf fullNameTxt
            line txt $"        let y = ProtoClasses.{fullNameTxt}()"

            for fieldInfo in info.Fields do
                genFieldToProtobuf $"x.{fieldInfo.Name}" $"y.{fieldInfo.Name}" fieldInfo

            line txt $"        y"
            line txt ""
        | Union union ->
            fromProtobuf (dottedName union.Name)
            line txt "        match x.UnionCase with"

            for case in union.Cases do
                let rightValue =
                    match case with
                    | Types.EmptyRecord -> $"{dottedName case.Name}"
                    | Types.SingleFieldRecord fieldInfo ->
                        match ProtoCmd.caseNeedsRecord case with
                        | true -> $"x.{firstName case.Name} |> Convert{solidName ns}.FromProtobuf"
                        | false -> $"{dottedName case.Name}(x.{firstName case.Name}{convertionFrom ns fieldInfo.Type})"
                    | Types.MultiFieldsRecord -> $"x.{firstName case.Name} |> Convert{solidName ns}.FromProtobuf"

                line
                    txt
                    $"        | ProtoClasses.{dottedName union.Name}.UnionOneofCase.{firstName case.Name} -> {rightValue}"

            line txt $"        | _ -> {dottedName union.Name}.Unknown"

            toProtobuf (dottedName union.Name)
            line txt $"        let y = ProtoClasses.{dottedName union.Name}()"
            line txt "        match x with"

            for case in union.Cases do
                let values = case.Fields |> List.map Common.getName |> String.concat ","

                let condition =
                    $"        | {dottedName case.Name}"
                    + if values <> "" then $" ({values})" else ""

                match case with
                | Types.EmptyRecord -> line txt $"{condition} -> y.{firstName case.Name} <- true"
                | Types.SingleFieldRecord fieldInfo ->
                    match ProtoCmd.caseNeedsRecord case with
                    | true ->
                        line
                            txt
                            ($"{condition} -> y.{firstName case.Name} <- "
                             + $"Convert{solidName ns}.{firstName union.Name}Case{firstName case.Name}ToProtobuf({values})")
                    | false ->
                        line txt $"{condition} ->"
                        genFieldToProtobuf fieldInfo.Name $"    y.{firstName case.Name}" fieldInfo
                | Types.MultiFieldsRecord ->
                    line
                        txt
                        ($"{condition} -> y.{firstName case.Name} <- "
                         + $"Convert{solidName ns}.{firstName union.Name}Case{firstName case.Name}ToProtobuf({values})")

            line txt $"        | {dottedName union.Name}.Unknown -> ()"
            line txt "        y"

            for case in union.Cases do
                match case with
                | Types.SingleFieldRecord fieldInfo ->
                    match fieldInfo.Type with
                    | Optional _
                    | Array _
                    | List _
                    | Set _
                    | Map _ -> genCase union case
                    | _ -> ()
                | Types.MultiFieldsRecord -> genCase union case
                | _ -> ()

            line txt ""

    and genCase (union: UnionInfo) (case: RecordInfo) =
        let fullCaseNameTxt = $"ProtoClasses.{dottedName union.Name}__{firstName case.Name}"

        let fieldsNames =
            case.Fields |> List.map (fun field -> field.Name) |> String.concat ","

        line txt $"    static member FromProtobuf (x:{fullCaseNameTxt}) ="

        let values =
            case.Fields
            |> List.map (fun fieldInfo -> $"({genFieldFromProtobuf fullCaseNameTxt fieldInfo})")
            |> String.concat ","

        line txt $"        {case.Name |> dottedName}"
        line txt $"            ({values})"
        line txt ""

        line
            txt
            $"    static member {firstName union.Name}Case{firstName case.Name}ToProtobuf ({fieldsNames}) : {fullCaseNameTxt} ="

        line txt $"        let y = {fullCaseNameTxt}()"

        for fieldInfo in case.Fields do
            genFieldToProtobuf $"{fieldInfo.Name}" $"y.{firstCharToUpper fieldInfo.Name}" fieldInfo

        line txt $"        y"
        line txt ""

    and genFieldFromProtobuf fullNameTxt fieldInfo =
        match fieldInfo.Type with
        | Optional t ->
            let caseValue =
                $"{fullNameTxt}.{firstCharToUpper fieldInfo.Name}OneofCase.{firstCharToUpper fieldInfo.Name}Value"

            $"if x.{firstCharToUpper fieldInfo.Name}Case = {caseValue} then ValueSome (x.{firstCharToUpper fieldInfo.Name}Value{convertionFrom ns t}) else ValueNone"
        | t -> $"x.{firstCharToUpper fieldInfo.Name}{convertionFrom ns t}"

    // TODO: support inner generic collections (e.g. Map<int32, Set<string>>)
    and genFieldToProtobuf xName yName fieldInfo =
        match fieldInfo.Type with
        | Optional t ->
            line txt $"        match {xName} with"
            line txt $"        | ValueSome v -> {yName}Value <- v{convertionTo t}"
            line txt $"        | ValueNone -> ()"
        | Array t
        | List t
        | Set t ->
            match fieldToProtobuf t with
            | Some cnv -> line txt $"        {yName}.AddRange({xName} |> Seq.map({cnv}))"
            | None -> line txt $"        {yName}.AddRange({xName})"
        | Map(k, v) ->
            let protoPairTypeName =
                $"ProtoClasses.{ns |> dottedName}.{ProtoCmd.keyValuePairName ns k v}"

            line txt $"        for pair in {xName} do"
            line txt $"            let protoPair = {protoPairTypeName} ()"
            line txt $"            protoPair.Key <- pair.Key{convertionTo k}"
            line txt $"            protoPair.Value <- pair.Value{convertionTo v}"
            line txt $"            {yName}.Add(protoPair)"
        | t -> line txt $"        {yName} <- {xName}{convertionTo t}"


    line txt $"namespace {genNamespace}"
    line txt ""
    line txt $"type Convert{solidName module'.Name}() ="
    line txt ""

    for item in module'.Items do
        genItem item

    txt.ToString()


let convertionFrom ns type' =
    fieldFromProtobuf ns type' |> Option.map ((+) " |> ") |> Option.defaultValue ""

let rec fieldFromProtobuf ns type' =
    match type' with
    | Bool
    | String -> None
    | Int8 -> Some "sbyte"
    | Int16 -> Some "int16"
    | Int32
    | Int64
    | Float32
    | Float64 -> None
    | Money scale -> Some $"fun v -> (decimal v) / {10. ** float (scale)}m"
    | Binary -> Some "fun v -> v.ToByteArray()"
    | Timestamp -> Some "fun v -> v.ToDateTime()"
    | Duration -> Some "fun v -> v.ToTimeSpan()"
    | Guid -> Some "fun v -> System.Guid(v.ToByteArray())"
    | Optional t -> failwith "direct convertion is not possible"
    | Array t ->
        match fieldFromProtobuf ns t with
        | Some convertion -> $"Seq.map({convertion}) |> Array.ofSeq"
        | None -> "Array.ofSeq"
        |> Some
    | List t ->
        match fieldFromProtobuf ns t with
        | Some convertion -> $"Seq.map({convertion}) |> List.ofSeq"
        | None -> "List.ofSeq"
        |> Some
    | Set t ->
        match fieldFromProtobuf ns t with
        | Some convertion -> $"Seq.map({convertion}) |> Set.ofSeq"
        | None -> "Set.ofSeq"
        |> Some
    | Map(k, v) ->
        $"Seq.map(fun x -> x.Key{convertionFrom ns k}, x.Value{convertionFrom ns v}) |> Map.ofSeq"
        |> Some
    | Complex typeName -> Some $"Convert{lastNames typeName |> solidName}.FromProtobuf"

let convertionTo type' =
    fieldToProtobuf type' |> Option.map ((+) " |> ") |> Option.defaultValue ""

let rec fieldToProtobuf type' =
    match type' with
    | Bool
    | String -> None
    | Int8 -> Some "int"
    | Int16 -> Some "int"
    | Int32
    | Int64
    | Float32
    | Float64 -> None
    | Money scale -> Some $"fun v -> int (v * {10. ** float (scale)}m)"
    | Binary -> Some "Google.Protobuf.ByteString.CopyFrom"
    | Timestamp -> Some "Google.Protobuf.WellKnownTypes.Timestamp.FromDateTime"
    | Duration -> Some "Google.Protobuf.WellKnownTypes.Duration.FromTimeSpan"
    | Guid -> Some "fun v -> Google.Protobuf.ByteString.CopyFrom(v.ToByteArray())"
    | Optional _ -> failwith "direct convertion is not possible"
    | Array _
    | List _
    | Set _
    | Map _ -> failwith "direct convertion is not supported, use AddRange"
    | Complex typeName -> Some $"Convert{lastNames typeName |> solidName}.ToProtobuf"
