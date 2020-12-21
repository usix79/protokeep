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
    assertOfString input {Name = ComplexName ["Foundation"; "Domain"]; Imports = []; Items = []}

[<Fact>]
let ``Test enum`` () =
    let input = """
module Domain
enum TrafficLight =
    | Red
    | Yellow
    | Green
"""
    assertOfString input {Name = ComplexName ["Domain"]; Imports = []; Items = [Enum {Name = ComplexName ["TrafficLight"; "Domain"]; Symbols = ["Red"; "Yellow"; "Green"]}]}

[<Fact>]
let ``Test single line enum`` () =
    let input = """
module Domain
enum TrafficLight = Red | Yellow | Green
"""
    assertOfString input {Name = ComplexName ["Domain"]; Imports = []; Items = [Enum {Name = ComplexName ["TrafficLight"; "Domain"]; Symbols = ["Red"; "Yellow"; "Green"]}]}

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
    Siblings: int list
    Props: string map
}
"""
    assertOfString input {
        Name = ComplexName ["Domain"]
        Imports = []
        Items = [
            Record {
                Name = ComplexName ["Crossroad"; "Domain"]
                Fields = [
                    { Name = "Id"; Type = Int; IsKey = false; Indexes = [] }
                    { Name = "LongId"; Type = Long; IsKey = false; Indexes = [] }
                    { Name = "AltId"; Type = Guid; IsKey = false; Indexes = [] }
                    { Name = "Street1"; Type = String; IsKey = false; Indexes = [] }
                    { Name = "Street2"; Type = String; IsKey = false; Indexes = [] }
                    { Name = "IsMonitored"; Type = Bool; IsKey = false; Indexes = [] }
                    { Name = "Xpos"; Type = Float; IsKey = false; Indexes = [] }
                    { Name = "Ypos"; Type = Double; IsKey = false; Indexes = [] }
                    { Name = "Ratio"; Type = Decimal 2; IsKey = false; Indexes = [] }
                    { Name = "LastChecked"; Type = Timestamp; IsKey = false; Indexes = [] }
                    { Name = "ServiceInterval"; Type = Duration; IsKey = false; Indexes = [] }
                    { Name = "CurrentLight"; Type = Complex (ComplexName ["TrafficLight"; "Domain"]); IsKey = false; Indexes = [] }
                    { Name = "Nickname"; Type = Optional String; IsKey = false; Indexes = [] }
                    { Name = "Img"; Type = Bytes; IsKey = false; Indexes = [] }
                    { Name = "Notes"; Type = Array String; IsKey = false; Indexes = [] }
                    { Name = "Siblings"; Type = List Int; IsKey = false; Indexes = [] }
                    { Name = "Props"; Type = Map String; IsKey = false; Indexes = [] }
                ]}]}

[<Fact>]
let ``Test single line record`` () =
    let input = """
module Domain
record Crossroad = { Id: int; Street1: string; Street2: string }
"""
    assertOfString input {
        Name = ComplexName ["Domain"]
        Imports = []
        Items = [
            Record {
                Name = ComplexName ["Crossroad"; "Domain"];
                Fields = [
                    { Name = "Id"; Type = Int; IsKey = false; Indexes = [] }
                    { Name = "Street1"; Type = String; IsKey = false; Indexes = [] }
                    { Name = "Street2"; Type = String; IsKey = false; Indexes = [] }
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
        Imports = []
        Items = [
            Union {
                Name = ComplexName ["ServiceCheck"; "Domain"];
                Cases = [
                    { Name = ComplexName ["Random";"ServiceCheck"; "Domain"]; Fields = []}
                    { Name = ComplexName ["Planned";"ServiceCheck"; "Domain"]; Fields = [{Name = "p1"; Type = Timestamp; IsKey = false; Indexes = []}] }
                    { Name = ComplexName ["Campaign";"ServiceCheck"; "Domain"]; Fields = [
                        {Name = "name"; Type = String; IsKey = false; Indexes = []}
                        {Name = "step"; Type = Int; IsKey = false; Indexes = []}
                        ]}
                    { Name = ComplexName ["RCA";"ServiceCheck"; "Domain"]; Fields = [{Name = "p1"; Type = Complex (ComplexName ["Incident"]); IsKey = false; Indexes = []}] }
                ]}]}

[<Fact>]
let ``Test single line union`` () =
    let input = """
module Domain
union ServiceCheck = Random | Planned of timestamp | Campaign of name:string*step:int | RCA of Incident
"""
    assertOfString input {
        Name = ComplexName ["Domain"]
        Imports = []
        Items = [
            Union {
                Name = ComplexName ["ServiceCheck"; "Domain"];
                Cases = [
                    { Name = ComplexName ["Random";"ServiceCheck"; "Domain"]; Fields = []}
                    { Name = ComplexName ["Planned";"ServiceCheck"; "Domain"]; Fields = [{Name = "p1"; Type = Timestamp; IsKey = false; Indexes = []}] }
                    { Name = ComplexName ["Campaign";"ServiceCheck"; "Domain"]; Fields = [
                        {Name = "name"; Type = String; IsKey = false; Indexes = []}
                        {Name = "step"; Type = Int; IsKey = false; Indexes = []}
                        ]}
                    { Name = ComplexName ["RCA";"ServiceCheck"; "Domain"]; Fields = [{Name = "p1"; Type = Complex (ComplexName ["Incident"]); IsKey = false; Indexes = []}] }
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
            Imports = []
            Items = [
                Enum {Name = ComplexName ["TrafficLight";"Foundation"; "Domain"]; Symbols = ["Red"; "Yellow"; "Green"]}
                Enum {
                    Name = ComplexName ["AltTrafficLight";"Foundation"; "Domain"]
                    Symbols = ["Red"; "Yellow"; "Blue"] }
                Record {
                    Name = ComplexName ["Crossroad";"Foundation"; "Domain"]
                    Fields =
                    [
                        { Name = "Id"; Type = Int; IsKey = false; Indexes = [] }
                        { Name = "Street1"; Type = String; IsKey = false; Indexes = [] }
                        { Name = "Street2"; Type = String; IsKey = false; Indexes = [] }
                    ]}
                Union {
                    Name = ComplexName ["ServiceCheck";"Foundation"; "Domain"]
                    Cases = [
                        { Name = ComplexName ["Random";"ServiceCheck";"Foundation"; "Domain"]; Fields = []}
                        { Name = ComplexName ["Planned";"ServiceCheck";"Foundation"; "Domain"]; Fields = [{ Name = "p1"; Type = Timestamp; IsKey = false; Indexes = [] }]}
                        { Name = ComplexName ["Campaign";"ServiceCheck";"Foundation"; "Domain"]; Fields = [
                            {Name = "name"; Type = String; IsKey = false; Indexes = []}
                            {Name = "step"; Type = Int; IsKey = false; Indexes = []}
                            ]}
                        { Name = ComplexName ["RCA";"ServiceCheck";"Foundation"; "Domain"]; Fields = [{Name = "p1"; Type = Complex (ComplexName ["Incident"]); IsKey = false; Indexes = []}] }
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
            Imports = []
            Items = [
                Record {
                    Name = ComplexName ["Crossroad";"Foundation"; "Domain"]
                    Fields =
                    [
                        { Name = "Id"; Type = Int; IsKey = true; Indexes = [] }
                        { Name = "Street1"; Type = String; IsKey = false; Indexes = [] }
                        { Name = "Street2"; Type = String; IsKey = true; Indexes = [] }
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
            Imports = []
            Items = [
                Record {
                    Name = ComplexName ["Crossroad";"Foundation"; "Domain"]
                    Fields =
                    [
                        { Name = "Id"; Type = Int; IsKey = true; Indexes = [] }
                        { Name = "Street1"; Type = String; IsKey = false; Indexes = [] }
                        { Name = "Street2"; Type = String; IsKey = true; Indexes = [] }
                    ]}
                Union {
                    Name = ComplexName ["U1";"Foundation"; "Domain"]
                    Cases = [
                        { Name = ComplexName ["Case1";"U1";"Foundation"; "Domain"]; Fields = []}
                        { Name = ComplexName ["Case2";"U1";"Foundation"; "Domain"]; Fields = [{ Name = "p1"; Type = Complex (ComplexName ["Crossroad"]); IsKey = false; Indexes = [] }]}
                        { Name = ComplexName ["Case3";"U1";"Foundation"; "Domain"]; Fields = [{ Name = "p1"; Type = Complex (ComplexName ["Crossroad"]); IsKey = true; Indexes = [] }]}
                        { Name = ComplexName ["Case4";"U1";"Foundation"; "Domain"]; Fields = [
                            {Name = "p1"; Type = Int; IsKey = true; Indexes = []}
                            {Name = "s1"; Type = String; IsKey = false; Indexes = []}
                            {Name = "s2"; Type = String; IsKey = true; Indexes = []}
                            ]}
                    ]}

                ]}

[<Fact>]
let IndexInRecordTest () =
    let input = """
module Domain.Foundation
record Crossroad = {
    Id: int key
    Street1: string idx
    Street2: string key idx
    Street3: string idx idx:street
    Street5: string idx[.Field1]
    Street6: string idx[.Field1 => .Field2]
}
"""
    assertOfString input
        {
            Name = ComplexName ["Foundation"; "Domain"]
            Imports = []
            Items = [
                Record {
                    Name = ComplexName ["Crossroad";"Foundation"; "Domain"]
                    Fields =
                    [
                        { Name = "Id"; Type = Int; IsKey = true; Indexes = [] }
                        { Name = "Street1"; Type = String; IsKey = false; Indexes = [{Name="item"; Key = Num; Value = Self}] }
                        { Name = "Street2"; Type = String; IsKey = true; Indexes = [{Name="item"; Key = Num; Value = Self}] }
                        { Name = "Street3"; Type = String; IsKey = false ; Indexes = [{Name="item"; Key = Num; Value = Self}; {Name="street"; Key = Num; Value = Self}] }
                        { Name = "Street5"; Type = String; IsKey = false; Indexes = [{Name="item"; Key = Num; Value = IndexValue.Field "Field1"}] }
                        { Name = "Street6"; Type = String; IsKey = false; Indexes = [{Name="item"; Key = IndexKey.FieldKey "Field1"; Value = IndexValue.Field "Field2"}] }
                    ]}
            ]
        }


    let input = """
module Domain
import "common.pgen"
enum TrafficLight =
    | Red
    | Yellow
    | Green
"""
    assertOfString input {Name = ComplexName ["Domain"]; Imports = ["common.pgen"]; Items = [Enum {Name = ComplexName ["TrafficLight"; "Domain"]; Symbols = ["Red"; "Yellow"; "Green"]}]}
