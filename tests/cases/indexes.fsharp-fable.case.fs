namespace Test.Converters

open Fable.SimpleJson
open Protokeep

type ConvertTestDomain () =
    static member ThreeWaysMarketFromJson (json: Json): Test.Domain.ThreeWaysMarket =
        let mutable vWin1 = 0m
        let mutable vDraw = 0m
        let mutable vWin2 = 0m
        FsharpFableHelpers.getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Win1" -> pair.Value |> FsharpFableHelpers.ifNumber (fun v -> vWin1 <- v |> unbox)
            | "Draw" -> pair.Value |> FsharpFableHelpers.ifNumber (fun v -> vDraw <- v |> unbox)
            | "Win2" -> pair.Value |> FsharpFableHelpers.ifNumber (fun v -> vWin2 <- v |> unbox)
            | _ -> () )
        {
            Win1 = vWin1
            Draw = vDraw
            Win2 = vWin2
        }
    static member ThreeWaysMarketToJson (x: Test.Domain.ThreeWaysMarket) =
        [
            "Win1", JNumber (unbox x.Win1)
            "Draw", JNumber (unbox x.Draw)
            "Win2", JNumber (unbox x.Win2)
        ] |> Map.ofList |> JObject
    static member TwoWaysMarketFromJson (json: Json): Test.Domain.TwoWaysMarket =
        let mutable vWin1 = 0m
        let mutable vWin2 = 0m
        FsharpFableHelpers.getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Win1" -> pair.Value |> FsharpFableHelpers.ifNumber (fun v -> vWin1 <- v |> unbox)
            | "Win2" -> pair.Value |> FsharpFableHelpers.ifNumber (fun v -> vWin2 <- v |> unbox)
            | _ -> () )
        {
            Win1 = vWin1
            Win2 = vWin2
        }
    static member TwoWaysMarketToJson (x: Test.Domain.TwoWaysMarket) =
        [
            "Win1", JNumber (unbox x.Win1)
            "Win2", JNumber (unbox x.Win2)
        ] |> Map.ofList |> JObject
    static member HandicapMarketFromJson (json: Json): Test.Domain.HandicapMarket =
        let mutable vValue = 0m
        let mutable vWin1 = 0m
        let mutable vWin2 = 0m
        FsharpFableHelpers.getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Value" -> pair.Value |> FsharpFableHelpers.ifNumber (fun v -> vValue <- v |> unbox)
            | "Win1" -> pair.Value |> FsharpFableHelpers.ifNumber (fun v -> vWin1 <- v |> unbox)
            | "Win2" -> pair.Value |> FsharpFableHelpers.ifNumber (fun v -> vWin2 <- v |> unbox)
            | _ -> () )
        {
            Value = vValue
            Win1 = vWin1
            Win2 = vWin2
        }
    static member HandicapMarketToJson (x: Test.Domain.HandicapMarket) =
        [
            "Value", JNumber (unbox x.Value)
            "Win1", JNumber (unbox x.Win1)
            "Win2", JNumber (unbox x.Win2)
        ] |> Map.ofList |> JObject
    static member MarketFromJson (json: Json): Test.Domain.Market =
        let mutable y = Test.Domain.Market.Unknown
        FsharpFableHelpers.getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "ThreeWaysMarket" ->
                let mutable _p1 = ThreeWaysMarket.Default.Value
                pair.Value |> (fun v -> _p1 <- v |> ConvertTestDomain.ThreeWaysMarketFromJson)
                y <- _p1 |> Test.Domain.Market.ThreeWaysMarket
            | "TwoWaysMarket" ->
                let mutable _p1 = TwoWaysMarket.Default.Value
                pair.Value |> (fun v -> _p1 <- v |> ConvertTestDomain.TwoWaysMarketFromJson)
                y <- _p1 |> Test.Domain.Market.TwoWaysMarket
            | "HandicapMarket" ->
                let mutable _p1 = HandicapMarket.Default.Value
                pair.Value |> (fun v -> _p1 <- v |> ConvertTestDomain.HandicapMarketFromJson)
                y <- _p1 |> Test.Domain.Market.HandicapMarket
            | _ -> () )
        y
    static member MarketToJson (x:Test.Domain.Market) =
        match x with
        | Test.Domain.Market.ThreeWaysMarket (p1) -> "ThreeWaysMarket", (p1 |> ConvertTestDomain.ThreeWaysMarketToJson)
        | Test.Domain.Market.TwoWaysMarket (p1) -> "TwoWaysMarket", (p1 |> ConvertTestDomain.TwoWaysMarketToJson)
        | Test.Domain.Market.HandicapMarket (p1) -> "HandicapMarket", (p1 |> ConvertTestDomain.HandicapMarketToJson)
        | _ -> "Unknown", JBool (true)
        |> List.singleton |> Map.ofList |> JObject
    static member ScoreFromJson (json: Json): Test.Domain.Score =
        let mutable vS1 = 0
        let mutable vS2 = 0
        FsharpFableHelpers.getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "S1" -> pair.Value |> FsharpFableHelpers.ifNumber (fun v -> vS1 <- v |> unbox)
            | "S2" -> pair.Value |> FsharpFableHelpers.ifNumber (fun v -> vS2 <- v |> unbox)
            | _ -> () )
        {
            S1 = vS1
            S2 = vS2
        }
    static member ScoreToJson (x: Test.Domain.Score) =
        [
            "S1", JNumber (unbox x.S1)
            "S2", JNumber (unbox x.S2)
        ] |> Map.ofList |> JObject
    static member GoalDetailsFromJson (json: Json): Test.Domain.GoalDetails =
        let mutable vScore = Score.Default.Value
        let mutable vComment = ""
        FsharpFableHelpers.getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Score" -> pair.Value |> (fun v -> vScore <- v |> ConvertTestDomain.ScoreFromJson)
            | "Comment" -> pair.Value |> FsharpFableHelpers.ifString (fun v -> vComment <- v)
            | _ -> () )
        {
            Score = vScore
            Comment = vComment
        }
    static member GoalDetailsToJson (x: Test.Domain.GoalDetails) =
        [
            "Score", (x.Score |> ConvertTestDomain.ScoreToJson)
            "Comment", JString (x.Comment)
        ] |> Map.ofList |> JObject
    static member MatchProtocolFromJson (json: Json): Test.Domain.MatchProtocol =
        let mutable vDetails = ResizeArray()
        FsharpFableHelpers.getProps json
        |> Seq.iter(fun pair ->
            match pair.Key with
            | "Details" -> pair.Value |> FsharpFableHelpers.ifArray (Seq.iter ((fun v -> v |> ConvertTestDomain.GoalDetailsFromJson |> vDetails.Add)))
            | _ -> () )
        {
            Details = vDetails |> List.ofSeq
        }
    static member MatchProtocolToJson (x: Test.Domain.MatchProtocol) =
        [
            "Details", JArray (x.Details |> Seq.map (fun v -> (v |> ConvertTestDomain.GoalDetailsToJson)) |> List.ofSeq)
        ] |> Map.ofList |> JObject
