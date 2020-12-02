module InputTests

open System
open Xunit
open Protogen.Input

let private assertParse input expected =
    match parse input with
    | Ok res -> Assert.Equal<Module list>(expected, res)
    | Error err -> failwith err

[<Fact>]
let ``Test empty input`` () =
    let input = ""
    assertParse input []

[<Fact>]
let ``Test single module`` () =
    let input = "module Domain.Foundation"
    assertParse input [{Name = ["Domain"; "Foundation"]}]


[<Fact>]
let ``Test many modules`` () =
    let input = """
module Domain
module Domain.Foundation
"""
    let expected = [
            {Name = ["Domain"]}
            {Name = ["Domain"; "Foundation"]}]

    assertParse input expected
