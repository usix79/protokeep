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
    assertParse "" []

[<Fact>]
let ``Test single empty module`` () =
    let input = "module Domain.Foundation"
    assertParse input [{Name = ComplexName ["Domain"; "Foundation"]; Items = []}]


[<Fact>]
let ``Test many empty modules`` () =
    let input = """
module Domain
module Domain.Foundation
"""
    let expected = [
            {Name = ComplexName ["Domain"]; Items = []}
            {Name = ComplexName ["Domain"; "Foundation"]; Items = []}]

    assertParse input expected

[<Fact>]
let ``Test enum`` () =
    let input = """
module Domain
enum TrafficLight =
    | Red
    | Yellow
    | Green
"""
    assertParse input [{Name = ComplexName ["Domain"]; Items = [Enum {Name = "TrafficLight"; Symbols = ["Red"; "Yellow"; "Green"]}]}]

[<Fact>]
let ``Test single line enum`` () =
    let input = """
module Domain
enum TrafficLight = Red | Yellow | Green
"""
    assertParse input [{Name = ComplexName ["Domain"]; Items = [Enum {Name = "TrafficLight"; Symbols = ["Red"; "Yellow"; "Green"]}]}]

[<Fact>]
let ``Test record`` () =
    let input = """
module Domain
record Crossroad = {
    Id: int
    LongId: long
    AltId: guid
    Street1: string
    Street2: string
    IsMonitored: bool
    Xpos: float
    Ypos: double
    Ratio: decimal(2)
    LastChecked: timestamp
    ServiceInterval: duration
    CurrentLight: Domain.TrafficLight
    Nickname: string option
    Img: bytes
    Notes: string array
    Props: string map
}
"""
    assertParse input [{
        Name = ComplexName ["Domain"]
        Items = [
            Record {
                Name = "Crossroad";
                Fields = [
                    { Name = "Id"; Type = Int }
                    { Name = "LongId"; Type = Long }
                    { Name = "AltId"; Type = Guid }
                    { Name = "Street1"; Type = String }
                    { Name = "Street2"; Type = String }
                    { Name = "IsMonitored"; Type = Bool }
                    { Name = "Xpos"; Type = Float }
                    { Name = "Ypos"; Type = Double }
                    { Name = "Ratio"; Type = Decimal 2 }
                    { Name = "LastChecked"; Type = Timespamp }
                    { Name = "ServiceInterval"; Type = Duration }
                    { Name = "CurrentLight"; Type = Complex (ComplexName ["Domain"; "TrafficLight"]) }
                    { Name = "Nickname"; Type = Optional String }
                    { Name = "Img"; Type = Bytes }
                    { Name = "Notes"; Type = Array String }
                    { Name = "Props"; Type = Map String }
                ]}]}]


[<Fact>]
let ``Test single line record`` () =
    let input = """
module Domain
record Crossroad = { Id: int; Street1: string; Street2: string }
"""
    assertParse input [{
        Name = ComplexName ["Domain"]
        Items = [
            Record {
                Name = "Crossroad";
                Fields = [
                    { Name = "Id"; Type = Int }
                    { Name = "Street1"; Type = String }
                    { Name = "Street2"; Type = String }
                ]}]}]


[<Fact>]
let ``Test union`` () =
    let input = """
module Domain
union ServiceCheck =
    | Random
    | Planned of timestamp
    | Campaign of name:string*step:int
    | RCA of Incident
"""
    assertParse input [{
        Name = ComplexName ["Domain"]
        Items = [
            Union {
                Name = "ServiceCheck";
                Cases = [
                    { Name = "Random"; Fields = []}
                    { Name = "Planned"; Fields = [{Name = None; Type = Timespamp}] }
                    { Name = "Campaign"; Fields = [
                        {Name = Some "name"; Type = String}
                        {Name = Some "step"; Type = Int}
                        ]}
                    { Name = "RCA"; Fields = [{Name = None; Type = Complex (ComplexName ["Incident"])}] }
                ]}]}]

[<Fact>]
let ``Test single line union`` () =
    let input = """
module Domain
union ServiceCheck = Random | Planned of timestamp | Campaign of name:string*step:int | RCA of Incident
"""
    assertParse input [{
        Name = ComplexName ["Domain"]
        Items = [
            Union {
                Name = "ServiceCheck";
                Cases = [
                    { Name = "Random"; Fields = []}
                    { Name = "Planned"; Fields = [{Name = None; Type = Timespamp}] }
                    { Name = "Campaign"; Fields = [
                        {Name = Some "name"; Type = String}
                        {Name = Some "step"; Type = Int}
                        ]}
                    { Name = "RCA"; Fields = [{Name = None; Type = Complex (ComplexName ["Incident"])}] }
                ]}]}]


[<Fact>]
let ``Complex Test`` () =
    let input = """
module Domain
enum TrafficLight = Red | Yellow | Green

module Domain.Foundation
enum AltTrafficLight = Red | Yellow | Blue
record Crossroad = { Id: int; Street1: string; Street2: string }
union ServiceCheck = Random | Planned of timestamp | Campaign of name:string*step:int | RCA of Incident
"""
    assertParse input [
        {   Name = ComplexName ["Domain"]
            Items = [Enum {Name = "TrafficLight"; Symbols = ["Red"; "Yellow"; "Green"]}] }
        {   Name = ComplexName ["Domain"; "Foundation"]
            Items = [
                Enum {
                    Name = "AltTrafficLight";
                    Symbols = ["Red"; "Yellow"; "Blue"] }
                Record { Name = "Crossroad";
                    Fields =
                    [
                        { Name = "Id"; Type = Int }
                        { Name = "Street1"; Type = String }
                        { Name = "Street2"; Type = String }
                    ]}
                Union {
                    Name = "ServiceCheck";
                    Cases = [
                        { Name = "Random"; Fields = []}
                        { Name = "Planned"; Fields = [{ Name = None; Type = Timespamp }]}
                        { Name = "Campaign"; Fields = [
                            {Name = Some "name"; Type = String}
                            {Name = Some "step"; Type = Int}
                            ]}
                        { Name = "RCA"; Fields = [{Name = None; Type = Complex (ComplexName ["Incident"])}] }
                    ]}
                ]}
    ]
