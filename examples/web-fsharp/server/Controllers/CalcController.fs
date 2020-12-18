namespace Server.Controllers

open System
open Microsoft.AspNetCore.Mvc
open Microsoft.Extensions.Logging
open System.IO
open FSharp.Control.Tasks.V2

open Protogen
open Domain
open Logic

[<ApiController>]
[<Route("[controller]")>]
type CalcController (logger : ILogger<CalcController>) =
    inherit ControllerBase()

    [<HttpGet>]
    member _.Get() = "Post valid request"

    [<HttpPost>]
    member x.Json() =
        task{
            use reader = new StreamReader(x.Request.Body)
            let! jsonReq = reader.ReadToEndAsync()

            let req =
                jsonReq
                |> Google.Protobuf.JsonParser.Default.Parse<ProtoClasses.Domain.Request>
                |> FsharpConverters.ConvertDomain.FromProtobuf

            let resp =
                {
                    Token = req.Token
                    Result =
                        match calc req.Operation with
                        | Ok v -> Success v
                        | Error v -> Fail v
                    ExecutionTime = TimeSpan.FromMilliseconds(1341.)
                    Extra = None
                    Since = DateTimeOffset.UtcNow
                    Tags = ["tag1", "AGA"; "tag2", "BG"; "tag3333", "Hello"] |> Map.ofList
                }

            return
                resp
                |> FsharpConverters.ConvertDomain.ToProtobuf
                |> Google.Protobuf.JsonFormatter.Default.Format
        }
