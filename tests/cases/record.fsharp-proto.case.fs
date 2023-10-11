namespace Test.Converters

type ConvertTestDomain() =

    static member FromProtobuf(x: ProtoClasses.Test.Domain.Crossroad) : Test.Domain.Crossroad =
        {
            Id = x.Id
            AltId = x.AltId |> fun v -> System.Guid(v.ToByteArray())
            Address = x.Address
            Corner = if x.CornerCase = ProtoClasses.Test.Domain.Crossroad.CornerOneofCase.CornerValue then ValueSome (x.CornerValue) else ValueNone
            IsMonitored = x.IsMonitored
            Patch = x.Patch |> sbyte
            Model = x.Model |> int16
            Serial = x.Serial
            Mask = x.Mask
            Cost = x.Cost |> fun v -> (decimal v) / 100m
            Xpos = x.Xpos
            Ypos = x.Ypos
            LastChecked = x.LastChecked |> fun v -> v.ToDateTime()
            ServiceInterval = x.ServiceInterval |> fun v -> v.ToTimeSpan()
            Intervals = x.Intervals |> List.ofSeq
            Notes = x.Notes |> Array.ofSeq
            Tags = x.Tags |> Seq.map(fun x -> x.Key, x.Value) |> Map.ofSeq
            Next = if x.NextCase = ProtoClasses.Test.Domain.Crossroad.NextOneofCase.NextValue then ValueSome (x.NextValue |> ConvertTestDomain.FromProtobuf) else ValueNone
            Img = x.Img |> fun v -> v.ToByteArray()
            Version = x.Version
        }

    static member ToProtobuf(x: Test.Domain.Crossroad) : ProtoClasses.Test.Domain.Crossroad =
        let y = ProtoClasses.Test.Domain.Crossroad()
        y.Id <- x.Id
        y.AltId <- x.AltId |> fun v -> Google.Protobuf.ByteString.CopyFrom(v.ToByteArray())
        y.Address <- x.Address
        match x.Corner with
        | ValueSome v -> y.CornerValue <- v
        | ValueNone -> ()
        y.IsMonitored <- x.IsMonitored
        y.Patch <- x.Patch |> int
        y.Model <- x.Model |> int
        y.Serial <- x.Serial
        y.Mask <- x.Mask
        y.Cost <- x.Cost |> fun v -> int (v * 100m)
        y.Xpos <- x.Xpos
        y.Ypos <- x.Ypos
        y.LastChecked <- x.LastChecked |> Google.Protobuf.WellKnownTypes.Timestamp.FromDateTime
        y.ServiceInterval <- x.ServiceInterval |> Google.Protobuf.WellKnownTypes.Duration.FromTimeSpan
        y.Intervals.AddRange(x.Intervals)
        y.Notes.AddRange(x.Notes)
        for pair in x.Tags do
            let protoPair = ProtoClasses.Test.Domain.StringInt32Pair ()
            protoPair.Key <- pair.Key
            protoPair.Value <- pair.Value
            y.Tags.Add(protoPair)
        match x.Next with
        | ValueSome v -> y.NextValue <- v |> ConvertTestDomain.ToProtobuf
        | ValueNone -> ()
        y.Img <- x.Img |> Google.Protobuf.ByteString.CopyFrom
        y.Version <- x.Version
        y

