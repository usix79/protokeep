open System
open System.IO
open System.Text
open System.Text.Json
open Google.Protobuf
open MongoDB.Bson
open MongoDB.Bson.IO
open MongoDB.Bson.Serialization
open MongoDB.Bson.Serialization.Serializers

open Protokeep
open Example.GameDomain

let req =
    { Game =
        { Id = Guid.NewGuid()
          Player = 7y
          Status = InProgress 1s
          Board =
            [ { X = 0y; Y = 0y }, { Unit.Name = "Knight"; Health = 100y }
              { X = 0y; Y = 2y }, { Unit.Name = "Archer"; Health = 70y }
              { X = 4y; Y = 2y }, { Unit.Name = "Wizard"; Health = 12y } ]
            |> Map.ofList
          LastChange = DateTime.Today.ToUniversalTime()
          Version = 1 }
      Action = Drop { X = 1y; Y = 2y } }

let respOk =
    Ok(
        { req.Game with
            Status = InProgress 2s
            Board = req.Game.Board.Add({ X = 1y; Y = 2y }, { Unit.Name = "Cleric"; Health = 100y })
            Version = 2 },
        [ Drop { X = 1y; Y = 1y }; Move({ X = 1y; Y = 2y }, { X = 1y; Y = 3y }) ]
    )

let respFail = Fail [ "Invalid action"; "Server error" ]

let serializeRequestToProtobuf (req: Example.GameDomain.Request) =
    use stream = new MemoryStream()
    use output = new CodedOutputStream(stream)
    let protoClass = FsharpProto.ConvertExampleGameDomain.ToProtobuf req
    protoClass.WriteTo(stream)
    output.Flush()
    stream.ToArray()

let deserializeRequestFromProtobuf (bytes: byte array) =
    use stream = new MemoryStream(bytes)
    let protoClass = ProtoClasses.Example.GameDomain.Request()
    protoClass.MergeFrom(stream)
    FsharpProto.ConvertExampleGameDomain.FromProtobuf protoClass

let serializeResponseToProtobuf (req: Example.GameDomain.Response) =
    use stream = new MemoryStream()
    use output = new CodedOutputStream(stream)
    let protoClass = FsharpProto.ConvertExampleGameDomain.ToProtobuf req
    protoClass.WriteTo(stream)
    output.Flush()
    stream.ToArray()

let deserializeResponseFromProtobuf (bytes: byte array) =
    use stream = new MemoryStream(bytes)
    let protoClass = ProtoClasses.Example.GameDomain.Response()
    protoClass.MergeFrom(stream)
    FsharpProto.ConvertExampleGameDomain.FromProtobuf protoClass

let checkProtobuf<'T when 'T: equality> (entity: 'T) (serialize: 'T -> byte array) (deserialize: byte array -> 'T) =
    let bytes = serialize entity
    printfn $"PROFOBUF: size = {bytes.Length} bytes:\n{BitConverter.ToString(bytes)}"
    let copy = deserialize bytes
    printfn $"PROTOBUF EUQALS: {entity = copy}"

let serializeRequestToJson (req: Example.GameDomain.Request) =
    use stream = new MemoryStream()
    let mutable writer = new Utf8JsonWriter(stream)
    FsharpJson.ConvertExampleGameDomain.RequestToJson(&writer, req)
    writer.Flush()
    let bytes = stream.ToArray()
    System.Text.Encoding.UTF8.GetString(bytes, 0, bytes.Length)

let deserializeRequestFromJson (json: string) =
    let mutable reader =
        Utf8JsonReader(ReadOnlySpan.op_Implicit (System.Text.Encoding.UTF8.GetBytes(json)))

    FsharpJson.ConvertExampleGameDomain.RequestFromJson(&reader)

let serializeResponseToJson (req: Example.GameDomain.Response) =
    use stream = new MemoryStream()
    let mutable writer = new Utf8JsonWriter(stream)
    FsharpJson.ConvertExampleGameDomain.ResponseToJson(&writer, req)
    writer.Flush()
    let bytes = stream.ToArray()
    System.Text.Encoding.UTF8.GetString(bytes, 0, bytes.Length)

let deserializeResponseFromJson (json: string) =
    let mutable reader =
        Utf8JsonReader(ReadOnlySpan.op_Implicit (System.Text.Encoding.UTF8.GetBytes(json)))

    FsharpJson.ConvertExampleGameDomain.ResponseFromJson(&reader)

let checkJson<'T when 'T: equality> (entity: 'T) (serialize: 'T -> string) (deserialize: string -> 'T voption) =
    let jsonString = serialize entity
    printfn $"JSON:  size = {Encoding.UTF8.GetByteCount(jsonString)} bytes:\n{jsonString}"

    match deserialize jsonString with
    | ValueSome copy ->
        printfn $"JSON EUQALS: {entity = copy}"

        if entity <> copy then
            printfn $"Copy: {copy}"
    | ValueNone -> printfn $"Failed to deserialize JSON"

let serializeRequestToBson (req: Example.GameDomain.Request) =
    use stream = new MemoryStream()
    use writer = new BsonBinaryWriter(stream)
    FsharpMongo.ConvertExampleGameDomain.RequestToBson(writer, req)
    writer.Flush()
    stream.ToArray()

let deserializeRequestFromBson (bytes: byte array) =
    use stream = new MemoryStream(bytes)
    use reader = new BsonBinaryReader(stream)
    FsharpMongo.ConvertExampleGameDomain.RequestFromBson(reader)

let serializeResponseToBson (req: Example.GameDomain.Response) =
    use stream = new MemoryStream()
    use writer = new BsonBinaryWriter(stream)
    FsharpMongo.ConvertExampleGameDomain.ResponseToBson(writer, req)
    writer.Flush()
    stream.ToArray()

let deserializeResponseFromBson (bytes: byte array) =
    use stream = new MemoryStream(bytes)
    use reader = new BsonBinaryReader(stream)
    FsharpMongo.ConvertExampleGameDomain.ResponseFromBson(reader)

let checkBson<'T when 'T: equality> (entity: 'T) (serialize: 'T -> byte array) (deserialize: byte array -> 'T) =
    let bytes = serialize entity

    use stream = new MemoryStream(bytes)
    use bsonReader = new BsonBinaryReader(stream)
    let context = BsonDeserializationContext.CreateRoot(bsonReader)
    let bsonDoc = BsonDocumentSerializer.Instance.Deserialize(context)

    printfn $"BSON: size = {bytes.Length} bytes:\n{bsonDoc.ToJson()}"
    let copy = deserialize bytes
    printfn $"BSON EUQALS: {entity = copy}"

    if entity <> copy then
        printfn $"Copy: {copy}"


FsharpMongo.ConvertExampleGameDomain.RegisterSerializers()

printfn $"Test request: {req}"
checkProtobuf req serializeRequestToProtobuf deserializeRequestFromProtobuf
checkJson req serializeRequestToJson deserializeRequestFromJson
checkBson req serializeRequestToBson deserializeRequestFromBson

printfn $"Positive response: {respOk}"
checkProtobuf respOk serializeResponseToProtobuf deserializeResponseFromProtobuf
checkJson respOk serializeResponseToJson deserializeResponseFromJson
checkBson respOk serializeResponseToBson deserializeResponseFromBson

printfn $"Negative response: {respFail}"
checkProtobuf respFail serializeResponseToProtobuf deserializeResponseFromProtobuf
checkJson respFail serializeResponseToJson deserializeResponseFromJson
checkBson respFail serializeResponseToBson deserializeResponseFromBson
