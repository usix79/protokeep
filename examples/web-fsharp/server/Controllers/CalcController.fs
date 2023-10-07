namespace Server.Controllers

open System
open Microsoft.AspNetCore.Mvc
open Microsoft.Extensions.Logging
open System.IO
open System.Text.Json

open Domain
open Domain.JsonConverters
open Logic

[<ApiController>]
[<Route("[controller]")>]
type CalcController(logger: ILogger<CalcController>) =
    inherit ControllerBase()

    [<HttpGet>]
    member _.Get() = "Post valid request"

    [<HttpPost>]
    member x.Json() =
        task {
            use reader = new StreamReader(x.Request.Body)
            let! jsonReq = reader.ReadToEndAsync()

            let mutable reader =
                Utf8JsonReader(ReadOnlySpan(System.Text.Encoding.UTF8.GetBytes(jsonReq)))

            let req = ConvertDomain.RequestFromJson(&reader)

            printfn $"{req}"

            let resp =
                { Token = req.Token
                  Result =
                    match calc req.Operation with
                    | Ok v -> Success v
                    | Error v -> Fail v
                  ExecutionTime = TimeSpan.FromMilliseconds(1341.)
                  Extra = ValueSome "Hello"
                  Since = DateTime.UtcNow
                  Tags = [ "tag1", "AGA"; "tag2", "BG"; "tag3333", "Hello" ] |> Map.ofList
                  Status = Subdomain.Status.Green }

            use stream = new MemoryStream()
            let mutable writer = new Utf8JsonWriter(stream)
            ConvertDomain.ResponseToJson(&writer, resp)
            writer.Flush()
            let data = stream.ToArray()
            let json = System.Text.Encoding.UTF8.GetString(data, 0, data.Length)

            return json
        }
