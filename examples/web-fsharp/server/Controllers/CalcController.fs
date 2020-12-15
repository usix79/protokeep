namespace Server.Controllers

open System
open Microsoft.AspNetCore.Mvc
open Microsoft.Extensions.Logging
open System.IO
open FSharp.Control.Tasks.V2

open Domain
open Logic

[<ApiController>]
[<Route("[controller]")>]
type CalcController (logger : ILogger<CalcController>) =
    inherit ControllerBase()

    [<HttpGet>]
    member _.Get() =
        """
Usage:
    /json - for json request
    /bin - for binary request
"""

    [<HttpPost>]
    [<Route("json")>]
    member x.Json() =
        task{
            use reader = new StreamReader(x.Request.Body)
            let! jsonReq = reader.ReadToEndAsync()

            let req =
                jsonReq
                |> Google.Protobuf.JsonParser.Default.Parse<ProtoClasses.Domain.Request>
                |> ProtoConverters.FsharpTypes.ConvertDomain.FromProtobuf

            let resp =
                {
                    Token = req.Token
                    Result =
                        match calc req.Operation with
                        | Ok v -> Success v
                        | Error v -> Fail v
                    ExecutionTime = TimeSpan.Zero
                    Extra = None
                }

            return
                resp
                |> ProtoConverters.FsharpTypes.ConvertDomain.ToProtobuf
                |> Google.Protobuf.JsonFormatter.Default.Format
        }
