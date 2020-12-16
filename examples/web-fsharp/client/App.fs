module App

open Browser
open Fetch
open Domain
open ProtoConverters.Fable
open Fable.SimpleJson

promise {
    let req = {
        Token = "WebClient-123"
        Operation = Sum (Val 2, Sum (Val 40, Mul (Val 100500, Zero)))
    }
    document.getElementById("reqObj").innerText <- sprintf "%A" req
    let json = ConvertDomain.RequestToJson req
    let jsonTxt = SimpleJson.toString json
    document.getElementById("reqJson").innerText <- jsonTxt

    let props = [
      Method HttpMethod.POST
      requestHeaders  [ContentType "application/json"]
      Body <| BodyInit.Case3 jsonTxt
    ]

    let! resp = fetch "https://localhost:5001/calc" props
    let! respJsonTxt = resp.text()

    document.getElementById("respJson").innerText <- respJsonTxt

    let respJson = SimpleJson.parse respJsonTxt
    let resp = ConvertDomain.ResponseFromJson respJson
    document.getElementById("respObj").innerText <- sprintf "%A" resp
} |> Promise.start