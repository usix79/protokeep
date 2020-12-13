open System
open System.IO
open Google.Protobuf
open Domain
open ProtoConverters.FsharpTypes

[<EntryPoint>]
let main argv =

    let crossroad = {
        Id = 124
        Street1 = "Bond st"
        Street2 = "Oxford st"
        Light = TrafficLight.Green
        LightStatus = LightStatus.OutOfOrder DateTimeOffset.UtcNow
    }

    printfn "Orig: %A" crossroad

    let protobufCrossroad = ConvertDomain.ToProtobuf crossroad
    use outputStream = new MemoryStream()
    use output = new CodedOutputStream(outputStream)
    protobufCrossroad.WriteTo(output)
    output.Flush()

    let bytes = outputStream.ToArray();
    printfn "Protobuf Bytes Length=%d, %s" bytes.Length (BitConverter.ToString(bytes))

    use inputStream = new MemoryStream(bytes)
    use input = new CodedInputStream(inputStream)
    let protoCopy = ProtoClasses.Domain.Crossroad()
    protoCopy.MergeFrom(input)
    let copy = ConvertDomain.FromProtobuf protoCopy
    printfn "Copy: %A" copy

    let json = JsonFormatter.Default.Format(protobufCrossroad)
    printfn "Json: %A" json

    let jsonCopy = JsonParser.Default.Parse<ProtoClasses.Domain.Crossroad>(json)
    let copyFromJson = ConvertDomain.FromProtobuf jsonCopy
    printfn "Copy from Json : %A" copyFromJson

    0 // return an integer exit code