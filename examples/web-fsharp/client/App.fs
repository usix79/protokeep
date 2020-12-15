module App

open Browser
open Fetch

promise {
    let props = [
      Method HttpMethod.POST
      requestHeaders  [ContentType "application/json"]
      Body <| BodyInit.Case3 """{ "Token": "Sigma-7", "OperationSum": { "P1Val": 2, "P2Mul": { "P1Val": 4, "P2Val": 10 } } }"""
    ]

    let! resp = fetch "https://localhost:5001/calc/json" props
    let! msg = resp.text()

    document.getElementById("header").innerText <- msg
} |> Promise.start