module App

open Browser
open Fetch

promise {
    let props = [
      Method HttpMethod.POST
      requestHeaders  [ContentType "application/json"]
      Body <| BodyInit.Case3 """{ "Token": "Sigma-7", "OperationSum": { "P1Val": { "P1": 2 }, "P2Mul": { "P1Val": { "P1": 4 }, "P2Val": { "P1": 10 } } } }"""
    ]

    let! resp = fetch "https://localhost:5001/calc/json" props
    let! msg = resp.text()

    document.getElementById("header").innerText <- msg
} |> Promise.start