namespace Test.Converters

open Fable.SimpleJson
open Protokeep

type ConvertTestDomain () =
    static member CrossroadFromJson (json: Json): Test.Domain.Crossroad =
        let mutable vId = 0
        let mutable vAltId = System.Guid.Empty
        let mutable vAddress = ""
        let mutable vCorner = ValueNone
        let mutable vIsMonitored = false
        let mutable vPatch = 0uy
        let mutable vModel = 0s
        let mutable vSerial = 0
        let mutable vMask = 0L
        let mutable vCost = 0m
        let mutable vXpos = 0.f
        let mutable vYpos = 0.
        let mutable vLastChecked = System.DateTime.MinValue
        let mutable vServiceInterval = System.TimeSpan.Zero
        let mutable vIntervals = ResizeArray()
        let mutable vNotes = ResizeArray()
        let mutable vTags = ResizeArray()
        let mutable vNext = ValueNone
        let mutable vImg = Array.empty
        let mutable vVersion = 0
        FsharpFableHelpers.getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Id" -> pair.Value |> FsharpFableHelpers.ifNumber (fun v -> vId <- v |> unbox)
            | "AltId" -> pair.Value |> FsharpFableHelpers.ifString (fun v -> vAltId <- v |> System.Convert.FromBase64String |> System.Guid)
            | "Address" -> pair.Value |> FsharpFableHelpers.ifString (fun v -> vAddress <- v)
            | "CornerValue" -> pair.Value |> FsharpFableHelpers.ifString (fun v -> vCorner <- v |> ValueSome)
            | "IsMonitored" -> pair.Value |> FsharpFableHelpers.ifBool (fun v -> vIsMonitored <- v)
            | "Patch" -> pair.Value |> FsharpFableHelpers.ifNumber (fun v -> vPatch <- v |> unbox)
            | "Model" -> pair.Value |> FsharpFableHelpers.ifNumber (fun v -> vModel <- v |> unbox)
            | "Serial" -> pair.Value |> FsharpFableHelpers.ifNumber (fun v -> vSerial <- v |> unbox)
            | "Mask" -> pair.Value |> FsharpFableHelpers.ifNumber (fun v -> vMask <- v |> unbox)
            | "Cost" -> pair.Value |> FsharpFableHelpers.ifNumber (fun v -> vCost <- v |> unbox)
            | "Xpos" -> pair.Value |> FsharpFableHelpers.ifNumber (fun v -> vXpos <- v |> unbox)
            | "Ypos" -> pair.Value |> FsharpFableHelpers.ifNumber (fun v -> vYpos <- v |> unbox)
            | "LastChecked" -> pair.Value |> FsharpFableHelpers.ifString (fun v -> vLastChecked <- v |> FsharpFableHelpers.toDateTime)
            | "ServiceInterval" -> pair.Value |> FsharpFableHelpers.ifString (fun v -> vServiceInterval <- v |> FsharpFableHelpers.toTimeSpan)
            | "Intervals" -> pair.Value |> FsharpFableHelpers.ifArray (Seq.iter (FsharpFableHelpers.ifNumber (fun v -> v |> unbox |> vIntervals.Add)))
            | "Notes" -> pair.Value |> FsharpFableHelpers.ifArray (Seq.iter (FsharpFableHelpers.ifString (fun v -> v |> vNotes.Add)))
            | "Tags" -> pair.Value |> FsharpFableHelpers.ifMap (fun (key, value) -> key |> FsharpFableHelpers.ifString (fun v -> v |> fun k -> value |> FsharpFableHelpers.ifNumber (fun v -> v |> unbox |> fun v -> vTags.Add(k, v))))
            | "NextValue" -> pair.Value |> (fun v -> vNext <- v |> ConvertTestDomain.CrossroadFromJson |> ValueSome)
            | "Img" -> pair.Value |> FsharpFableHelpers.ifString (fun v -> vImg <- v |> System.Convert.FromBase64String)
            | "Version" -> pair.Value |> FsharpFableHelpers.ifNumber (fun v -> vVersion <- v |> unbox)
            | _ -> () )
        {
            Id = vId
            AltId = vAltId
            Address = vAddress
            Corner = vCorner
            IsMonitored = vIsMonitored
            Patch = vPatch
            Model = vModel
            Serial = vSerial
            Mask = vMask
            Cost = vCost
            Xpos = vXpos
            Ypos = vYpos
            LastChecked = vLastChecked
            ServiceInterval = vServiceInterval
            Intervals = vIntervals |> List.ofSeq
            Notes = unbox vNotes
            Tags = vTags |> Map.ofSeq
            Next = vNext
            Img = vImg
            Version = vVersion
        }
    static member CrossroadToJson (x: Test.Domain.Crossroad) =
        [
            "Id", JNumber (unbox x.Id)
            "AltId", JString (x.AltId.ToByteArray() |> System.Convert.ToBase64String)
            "Address", JString (x.Address)
            match x.Corner with
            | ValueSome v -> "{firstCharToUpper fieldInfo.Name}Value", JString (v)
            | ValueNone -> ()
            "IsMonitored", JBool (x.IsMonitored)
            "Patch", JNumber (unbox x.Patch)
            "Model", JNumber (unbox x.Model)
            "Serial", JNumber (unbox x.Serial)
            "Mask", JNumber (unbox x.Mask)
            "Cost", JNumber (unbox x.Cost)
            "Xpos", JNumber (unbox x.Xpos)
            "Ypos", JNumber (unbox x.Ypos)
            "LastChecked", JString (x.LastChecked |> FsharpFableHelpers.fromDateTime)
            "ServiceInterval", JString (x.ServiceInterval |> FsharpFableHelpers.fromTimeSpan)
            "Intervals", JArray (x.Intervals |> Seq.map (fun v -> JNumber (unbox v)) |> List.ofSeq)
            "Notes", JArray (x.Notes |> Seq.map (fun v -> JString (v)) |> List.ofSeq)
            "Tags", x.Tags |> FsharpFableHelpers.mapToArray (fun k -> JString (k)) (fun v -> JNumber (unbox v))
            match x.Next with
            | ValueSome v -> "{firstCharToUpper fieldInfo.Name}Value", (v |> ConvertTestDomain.CrossroadToJson)
            | ValueNone -> ()
            "Img", JString (x.Img |> System.Convert.ToBase64String)
            "Version", JNumber (unbox x.Version)
        ] |> Map.ofList |> JObject
