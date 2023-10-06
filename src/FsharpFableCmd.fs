[<RequireQualifiedAccess>]
module rec Protokeep.FsharpFableCmd

open System.Text
open Types
open Codegen

let helpers = "FsharpFableHelpers"

let Handler module' locks typesCache =
    function
    | "-o" :: outputFileName :: args
    | "--output" :: outputFileName :: args ->
        Infra.checkLock module' locks typesCache
        |> Result.bind
           ^ fun _ ->
               let ns = Infra.checkArgNamespace args |> Option.defaultValue "Protokeep.FsharpFable"

               let fileName =
                   gen ns module' locks typesCache |> Infra.writeFile outputFileName ".fs"

               FsharpHelpers.update helpers |> Infra.updateCommons fileName args
    | x -> Error $"expected arguments [-o|--output] outputFile, but {x}"

let Instance =
    { Name = "fsharp-fable"
      Description = "generate converters between json and fsharp types in fable environment"
      Run = Handler }

let gen genNamespace (module': Module) (locks: LocksCollection) (typesCache: Types.TypesCache) =

    let txt = StringBuilder()

    let ns = module'.Name

    let rec genItem =
        function
        | Enum info ->
            let fullNameTxt = info.Name |> dottedName
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
            line txt $"        {helpers}.getProps json"
            line txt $"        |> Seq.iter(fun pair ->"
            line txt $"            match pair.Key with"

            for case in union.Cases do
                let rightValue =
                    match case with
                    | Types.EmptyRecord ->
                        $"{helpers}.ifBool (fun v -> y <- {dottedName union.Name}.{firstName case.Name})"
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
            | Types.IsEnum typesCache enumInfo ->
                line txt $"        let mutable {prefix}{fieldInfo.Name} = {dottedName enumInfo.Name}.Unknown"
            | Types.IsUnion typesCache unionInfo ->
                line txt $"        let mutable {prefix}{fieldInfo.Name} = {dottedName unionInfo.Name}.Unknown"
            | _ ->
                line txt $"        let mutable {prefix}{fieldInfo.Name} = {FsharpTypesCmd.defValue true fieldInfo.Type}"

        line txt $"        {helpers}.getProps json"
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

    line txt $"namespace {genNamespace}"
    line txt $"open Fable.SimpleJson"
    line txt $"open Protokeep"
    line txt $""
    line txt $"type Convert{solidName module'.Name} () ="

    for item in module'.Items do
        genItem item

    txt.ToString()

let unpackField' rightOp (typesCache: Types.TypesCache) vName =
    let rec f leftOp rightOp =
        function
        | Bool -> $"{helpers}.ifBool (fun v -> {leftOp}v{rightOp})"
        | String -> $"{helpers}.ifString (fun v -> {leftOp}v{rightOp})"
        | Int
        | Long
        | Float
        | Double -> $"{helpers}.ifNumber (fun v -> {leftOp}v |> unbox{rightOp})"
        | Decimal scale -> $"{helpers}.ifNumber (fun v -> {leftOp}v / {10. ** float (scale)}. |> unbox{rightOp})"
        | Bytes -> $"{helpers}.ifString (fun v -> {leftOp}v |> System.Convert.FromBase64String{rightOp})"
        | Timestamp -> $"{helpers}.ifString (fun v -> {leftOp}v |> {helpers}.toDateTime{rightOp})"
        | Duration -> $"{helpers}.ifString (fun v -> {leftOp}v |> {helpers}.toTimeSpan{rightOp})"
        | Guid -> $"{helpers}.ifString (fun v -> {leftOp}v |> System.Convert.FromBase64String |> System.Guid{rightOp})"
        | Optional t -> f $"{vName} <- " " |> Some" t
        | Array t
        | List t ->
            let inner = f "" $" |> {vName}.Add" t
            $"{helpers}.ifArray (Seq.iter ({inner}))"
        | Map t ->
            let inner = f "" $" |> fun v -> {vName}.Add(key, v)" t
            $"{helpers}.ifObject (Map.iter (fun key -> {inner}))"
        | Complex typeName ->
            match typesCache.TryFind typeName with
            | Some(Enum _) ->
                $"{helpers}.ifString (fun v -> {leftOp}v |> Convert{lastNames typeName |> solidName}.{firstName typeName}FromString{rightOp})"
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
        | Timestamp -> $"JString ({vName} |> {helpers}.fromDateTime)"
        | Duration -> $"JString ({vName} |> {helpers}.fromTimeSpan)"
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