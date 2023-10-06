open System
open System.IO

open MongoDB.Bson
open MongoDB.Bson.IO
open Domain


[<EntryPoint>]
let main argv =

    ConvertDomain.RegisterSerializers()

    let crossroad =
        { Id = 124
          LongId = 124L
          AltId = System.Guid.NewGuid()
          Street1 = "Bond st"
          Street2 = "Oxford st"
          IsMonitored = true
          Xpos = 12.3f
          Ypos = 32.1
          Ratio = 1.23M
          LastChecked = DateTime((DateTime.UtcNow.Ticks / 10000L) * 10000L)
          ServiceInterval = TimeSpan.FromMinutes(5.)
          CurrentLight = TrafficLight.Green
          Nickname = Some "Bond"
          Img = [| 0uy; 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy; 8uy; 9uy |]
          Notes = [| "Note1"; "Note2"; "Note3" |]
          Props = Map.ofList [ ("Prop1", "Value1"); ("Prop2", "Value2") ] }


    printfn "Orig: %A" crossroad

    use serializeStream = new MemoryStream()

    use writer = new BsonBinaryWriter(serializeStream)
    ConvertDomain.CrossroadToBson(writer, crossroad)
    writer.Flush()

    use deserializeStream = new MemoryStream(serializeStream.ToArray())
    use reader = new BsonBinaryReader(deserializeStream)
    let deserialisedCrossroad = ConvertDomain.CrossroadFromBson(reader)

    printfn "Copy:  %A" deserialisedCrossroad
    printfn "Equals: %b" (crossroad = deserialisedCrossroad)

    0
