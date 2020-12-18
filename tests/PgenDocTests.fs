module PgenDocTests

open System
open Xunit
open Protogen.Types
open Protogen

let private assertOfString input expected =
    match Parsers.parsePgenDoc input with
    | Ok res -> Assert.Equal<Module>(expected, res)
    | Error err -> failwith err

[<Fact>]
let ``Test single empty module`` () =
    let input = "module Domain.Foundation"
    assertOfString input {Name = ComplexName ["Foundation"; "Domain"]; Items = []}

[<Fact>]
let ``Test enum`` () =
    let input = """
module Domain
enum TrafficLight =
    | Red
    | Yellow
    | Green
"""
    assertOfString input {Name = ComplexName ["Domain"]; Items = [Enum {Name = ComplexName ["TrafficLight"; "Domain"]; Symbols = ["Red"; "Yellow"; "Green"]}]}

[<Fact>]
let ``Test single line enum`` () =
    let input = """
module Domain
enum TrafficLight = Red | Yellow | Green
"""
    assertOfString input {Name = ComplexName ["Domain"]; Items = [Enum {Name = ComplexName ["TrafficLight"; "Domain"]; Symbols = ["Red"; "Yellow"; "Green"]}]}

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
    assertOfString input {
        Name = ComplexName ["Domain"]
        Items = [
            Record {
                Name = ComplexName ["Crossroad"; "Domain"]
                Fields = [
                    { Name = "Id"; Type = Int; IsKey = false }
                    { Name = "LongId"; Type = Long; IsKey = false }
                    { Name = "AltId"; Type = Guid; IsKey = false }
                    { Name = "Street1"; Type = String; IsKey = false }
                    { Name = "Street2"; Type = String; IsKey = false }
                    { Name = "IsMonitored"; Type = Bool; IsKey = false }
                    { Name = "Xpos"; Type = Float; IsKey = false }
                    { Name = "Ypos"; Type = Double; IsKey = false }
                    { Name = "Ratio"; Type = Decimal 2; IsKey = false }
                    { Name = "LastChecked"; Type = Timestamp; IsKey = false }
                    { Name = "ServiceInterval"; Type = Duration; IsKey = false }
                    { Name = "CurrentLight"; Type = Complex (ComplexName ["TrafficLight"; "Domain"]); IsKey = false }
                    { Name = "Nickname"; Type = Optional String; IsKey = false }
                    { Name = "Img"; Type = Bytes; IsKey = false }
                    { Name = "Notes"; Type = Array String; IsKey = false }
                    { Name = "Props"; Type = Map String; IsKey = false }
                ]}]}

[<Fact>]
let ``Test single line record`` () =
    let input = """
module Domain
record Crossroad = { Id: int; Street1: string; Street2: string }
"""
    assertOfString input {
        Name = ComplexName ["Domain"]
        Items = [
            Record {
                Name = ComplexName ["Crossroad"; "Domain"];
                Fields = [
                    { Name = "Id"; Type = Int; IsKey = false }
                    { Name = "Street1"; Type = String; IsKey = false }
                    { Name = "Street2"; Type = String; IsKey = false }
                ]}]}

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
    assertOfString input {
        Name = ComplexName ["Domain"]
        Items = [
            Union {
                Name = ComplexName ["ServiceCheck"; "Domain"];
                Cases = [
                    { Name = ComplexName ["Random";"ServiceCheck"; "Domain"]; Fields = []}
                    { Name = ComplexName ["Planned";"ServiceCheck"; "Domain"]; Fields = [{Name = "p1"; Type = Timestamp; IsKey = false}] }
                    { Name = ComplexName ["Campaign";"ServiceCheck"; "Domain"]; Fields = [
                        {Name = "name"; Type = String; IsKey = false}
                        {Name = "step"; Type = Int; IsKey = false}
                        ]}
                    { Name = ComplexName ["RCA";"ServiceCheck"; "Domain"]; Fields = [{Name = "p1"; Type = Complex (ComplexName ["Incident"]); IsKey = false}] }
                ]}]}

[<Fact>]
let ``Test single line union`` () =
    let input = """
module Domain
union ServiceCheck = Random | Planned of timestamp | Campaign of name:string*step:int | RCA of Incident
"""
    assertOfString input {
        Name = ComplexName ["Domain"]
        Items = [
            Union {
                Name = ComplexName ["ServiceCheck"; "Domain"];
                Cases = [
                    { Name = ComplexName ["Random";"ServiceCheck"; "Domain"]; Fields = []}
                    { Name = ComplexName ["Planned";"ServiceCheck"; "Domain"]; Fields = [{Name = "p1"; Type = Timestamp; IsKey = false}] }
                    { Name = ComplexName ["Campaign";"ServiceCheck"; "Domain"]; Fields = [
                        {Name = "name"; Type = String; IsKey = false}
                        {Name = "step"; Type = Int; IsKey = false}
                        ]}
                    { Name = ComplexName ["RCA";"ServiceCheck"; "Domain"]; Fields = [{Name = "p1"; Type = Complex (ComplexName ["Incident"]); IsKey = false}] }
                ]}]}

[<Fact>]
let ``Complex Test`` () =
    let input = """
module Domain.Foundation

enum TrafficLight = Red | Yellow | Green

enum AltTrafficLight = Red | Yellow | Blue

record Crossroad = {
    Id: int
    Street1: string
    Street2: string
}

union ServiceCheck = Random | Planned of timestamp | Campaign of name:string*step:int | RCA of Incident

"""
    assertOfString input
        {   Name = ComplexName ["Foundation"; "Domain"]
            Items = [
                Enum {Name = ComplexName ["TrafficLight";"Foundation"; "Domain"]; Symbols = ["Red"; "Yellow"; "Green"]}
                Enum {
                    Name = ComplexName ["AltTrafficLight";"Foundation"; "Domain"]
                    Symbols = ["Red"; "Yellow"; "Blue"] }
                Record {
                    Name = ComplexName ["Crossroad";"Foundation"; "Domain"]
                    Fields =
                    [
                        { Name = "Id"; Type = Int; IsKey = false }
                        { Name = "Street1"; Type = String; IsKey = false }
                        { Name = "Street2"; Type = String; IsKey = false }
                    ]}
                Union {
                    Name = ComplexName ["ServiceCheck";"Foundation"; "Domain"]
                    Cases = [
                        { Name = ComplexName ["Random";"ServiceCheck";"Foundation"; "Domain"]; Fields = []}
                        { Name = ComplexName ["Planned";"ServiceCheck";"Foundation"; "Domain"]; Fields = [{ Name = "p1"; Type = Timestamp; IsKey = false }]}
                        { Name = ComplexName ["Campaign";"ServiceCheck";"Foundation"; "Domain"]; Fields = [
                            {Name = "name"; Type = String; IsKey = false}
                            {Name = "step"; Type = Int; IsKey = false}
                            ]}
                        { Name = ComplexName ["RCA";"ServiceCheck";"Foundation"; "Domain"]; Fields = [{Name = "p1"; Type = Complex (ComplexName ["Incident"]); IsKey = false}] }
                    ]}
                ]}

[<Fact>]
let KeyInRecordTest () =
    let input = """
module Domain.Foundation
record Crossroad = {
    Id: int key
    Street1: string
    Street2: string key
}
"""
    assertOfString input
        {   Name = ComplexName ["Foundation"; "Domain"]
            Items = [
                Record {
                    Name = ComplexName ["Crossroad";"Foundation"; "Domain"]
                    Fields =
                    [
                        { Name = "Id"; Type = Int; IsKey = true }
                        { Name = "Street1"; Type = String; IsKey = false }
                        { Name = "Street2"; Type = String; IsKey = true }
                    ]}
                ]}

[<Fact>]
let KeyInUnionTest () =
    let input = """
module Domain.Foundation
record Crossroad = {
    Id: int key
    Street1: string
    Street2: string key
}
union U1 =
    | Case1
    | Case2 of Crossroad
    | Case3 of Crossroad key
    | Case4 of int key * s1:string * s2:string key
"""
    assertOfString input
        {   Name = ComplexName ["Foundation"; "Domain"]
            Items = [
                Record {
                    Name = ComplexName ["Crossroad";"Foundation"; "Domain"]
                    Fields =
                    [
                        { Name = "Id"; Type = Int; IsKey = true }
                        { Name = "Street1"; Type = String; IsKey = false }
                        { Name = "Street2"; Type = String; IsKey = true }
                    ]}
                Union {
                    Name = ComplexName ["U1";"Foundation"; "Domain"]
                    Cases = [
                        { Name = ComplexName ["Case1";"U1";"Foundation"; "Domain"]; Fields = []}
                        { Name = ComplexName ["Case2";"U1";"Foundation"; "Domain"]; Fields = [{ Name = "p1"; Type = Complex (ComplexName ["Crossroad"]); IsKey = false }]}
                        { Name = ComplexName ["Case3";"U1";"Foundation"; "Domain"]; Fields = [{ Name = "p1"; Type = Complex (ComplexName ["Crossroad"]); IsKey = true }]}
                        { Name = ComplexName ["Case4";"U1";"Foundation"; "Domain"]; Fields = [
                            {Name = "p1"; Type = Int; IsKey = true}
                            {Name = "s1"; Type = String; IsKey = false}
                            {Name = "s2"; Type = String; IsKey = true}
                            ]}
                    ]}

                ]}