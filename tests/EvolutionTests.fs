module EvolutionTests

open System
open Xunit
open Xunit.Sdk
open Protogen.Types


let assertEqual expected actual =
    if expected <> actual then
        NotEqualException (sprintf "%A" expected, sprintf "%A" actual) |> raise

[<Fact>]
let ``Test empty evolution`` () =
    Assert.Equal(Ok [], (Evolution.lock [] []))

[<Fact>]
let ``Duplicate Enums`` () =
    let input = [{Name = ComplexName ["Domain"]; Items = [
        Enum {Name = "TrafficLight"; Symbols = ["Red"; "Yellow"; "Green"]}
        Enum {Name = "TrafficLight"; Symbols = ["Red"; "Yellow"; "Green"]}]}]
    let lock = []
    let expected = Error [ Evolution.DuplicateTypeNames (ComplexName ["TrafficLight"; "Domain"])]

    assertEqual expected (Evolution.lock input lock)

[<Fact>]
let ``Duplicate Locked Enums`` () =
    let input = [{Name = ComplexName ["Domain"]; Items = [
        Enum {Name = "TrafficLight"; Symbols = ["Red"; "Yellow"; "Green"; "Yellow"]}
        ]}]
    let lock = [
        EnumLock {
            Name = ComplexName ["TrafficLight"; "Domain"]
            Values = [
                {Name = "Red"; Num = 1}
                {Name = "Yellow"; Num = 2}
                {Name = "Green"; Num = 3}
            ]}
        EnumLock {
            Name = ComplexName ["TrafficLight"; "Domain"]
            Values = [
                {Name = "Red1"; Num = 1}
                {Name = "Yellow1"; Num = 2}
                {Name = "Green1"; Num = 3}
            ]}]
    let expected = Error [ Evolution.DuplicateLockedTypeNames (ComplexName ["TrafficLight"; "Domain"])]

    assertEqual expected (Evolution.lock input lock)

[<Fact>]
let ``Duplicate Symbols in Enum`` () =
    let input = [{Name = ComplexName ["Domain"]; Items = [
        Enum {Name = "TrafficLight"; Symbols = ["Red"; "Yellow"; "Green"; "Yellow"]}
        ]}]
    let lock = []
    let expected = Error [ Evolution.DuplicateSymbolsInEnum (ComplexName ["TrafficLight"; "Domain"], "Yellow")]

    assertEqual expected (Evolution.lock input lock)

[<Fact>]
let ``Duplicate Symbols in Locked Enum`` () =
    let input = [{Name = ComplexName ["Domain"]; Items = [
        Enum {Name = "TrafficLight"; Symbols = ["Red"; "Yellow"; "Green"]}
        ]}]
    let lock = [
        EnumLock {
            Name = ComplexName ["TrafficLight"; "Domain"]
            Values = [
                {Name = "Red"; Num = 1}
                {Name = "Yellow"; Num = 2}
                {Name = "Green"; Num = 3}
                {Name = "Yellow"; Num = 4}
            ]}]
    let expected = Error [ Evolution.DuplicateSymbolsInLockedEnum (ComplexName ["TrafficLight"; "Domain"], "Yellow")]

    assertEqual expected (Evolution.lock input lock)

[<Fact>]
let ``Missed Symbol in Enum`` () =
    let input = [{Name = ComplexName ["Domain"]; Items = [
        Enum {Name = "TrafficLight"; Symbols = ["Red"; "Yellow"; "Green"]}
        ]}]
    let lock = [
        EnumLock {
            Name = ComplexName ["TrafficLight"; "Domain"]
            Values = [
                {Name = "Red"; Num = 1}
                {Name = "Yellow"; Num = 2}
                {Name = "Green"; Num = 3}
                {Name = "Blue"; Num = 4}
            ]}]
    let expected = Error [ Evolution.MissedSymbolInEnum (ComplexName ["TrafficLight"; "Domain"], "Blue")]

    assertEqual expected (Evolution.lock input lock)

[<Fact>]
let ``Single Enum`` () =
    let input = [{Name = ComplexName ["Domain"]; Items = [Enum {Name = "TrafficLight"; Symbols = ["Red"; "Yellow"; "Green"]}]}]
    let lock = []
    let expected = Ok [
        EnumLock {
            Name= ComplexName ["TrafficLight"; "Domain"]
            Values = [
                {Name = "Red"; Num = 1}
                {Name = "Yellow"; Num = 2}
                {Name = "Green"; Num = 3}
            ]}
        ]

    assertEqual expected (Evolution.lock input lock)

// [<Fact>]
// let ``Unknown type of record's field`` () =
//     let input = [{
//         Name = ComplexName ["Domain"]
//         Items = [
//             Record {Name = "Simple"; Fields = [{Name= "Id"; Type = Complex (ComplexName ["XXX"])}]}
//         ]}]
//     let lock = []
//     let expected = Error [ Evolution.UnknownFieldType (ComplexName ["Simple"; "Domain"], "Id", ComplexName ["XXX"])]

//     assertEqual expected (Evolution.lock input lock)