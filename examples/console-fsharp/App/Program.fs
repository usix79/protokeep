open System
open System.IO
open System.Text.Json
open Google.Protobuf
open Domain
open Protokeep

[<EntryPoint>]
let main argv =

    let crossroad =
        { Id = 124
          Street1 = "Bond st"
          Street2 = "Oxford st"
          Light = TrafficLight.Green
          LightStatus = LightStatus.OutOfOrder DateTime.UtcNow
          History =
            [ LightStatus.Normal
              LightStatus.Warning 1
              LightStatus.Warning 3
              LightStatus.OutOfOrder DateTime.UtcNow ]
          Lirycs = [ "Words"; "Words"; "Words" ] }

    printfn "Orig: %A" crossroad

    let protobufCrossroad = FsharpProto.ConvertDomain.ToProtobuf crossroad
    use outputStream = new MemoryStream()
    use output = new CodedOutputStream(outputStream)
    protobufCrossroad.WriteTo(output)
    output.Flush()

    let bytes = outputStream.ToArray()
    printfn "Protobuf Bytes Length=%d, %s" bytes.Length (BitConverter.ToString(bytes))

    use inputStream = new MemoryStream(bytes)
    use input = new CodedInputStream(inputStream)
    let protoCopy = ProtoClasses.Domain.Crossroad()
    protoCopy.MergeFrom(input)
    let copy = FsharpProto.ConvertDomain.FromProtobuf protoCopy
    printfn "Copy: %A" copy

    let protoJson = JsonFormatter.Default.Format(protobufCrossroad)
    printfn "Protofuf Json: %A" protoJson

    use stream = new MemoryStream()
    let mutable writer = new Utf8JsonWriter(stream)
    FsharpJson.ConvertDomain.CrossroadToJson(&writer, crossroad)
    writer.Flush()
    let data = stream.ToArray()
    let json = System.Text.Encoding.UTF8.GetString(data, 0, data.Length)
    printfn "Protokeep Json: %A" json

    let mutable reader =
        Utf8JsonReader(ReadOnlySpan.op_Implicit (System.Text.Encoding.UTF8.GetBytes(json)))

    let copyFromJson = FsharpJson.ConvertDomain.CrossroadFromJson(&reader)
    printfn "Copy from Json : %A" copyFromJson

    let protoCopyFromJson =
        JsonParser.Default.Parse<ProtoClasses.Domain.Crossroad>(json)

    let copyFromJsonProto = FsharpProto.ConvertDomain.FromProtobuf protoCopyFromJson

    printfn "Copy from Proto: %A" copyFromJsonProto

    0 // return an integer exit code
