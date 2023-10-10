module ParserTests

open Xunit
open Protokeep.Types
open Protokeep

let private assertOfString input expected =
    match Parsers.parsePkDoc input with
    | Ok res -> Assert.Equal<Module>(expected, res)
    | Error err -> failwith err

[<Fact>]
let ``Test single empty module`` () =
    let input = "module Domain.Foundation"

    assertOfString
        input
        { Name = ComplexName [ "Foundation"; "Domain" ]
          Imports = []
          Items = [] }

[<Fact>]
let ``Test enum`` () =
    let input =
        """
module Domain
enum TrafficLight =
    | Red
    | Yellow
    | Green
"""

    assertOfString
        input
        { Name = ComplexName [ "Domain" ]
          Imports = []
          Items =
            [ Enum
                  { Name = ComplexName [ "TrafficLight"; "Domain" ]
                    Symbols = [ "Red"; "Yellow"; "Green" ] } ] }

[<Fact>]
let ``Test single line enum`` () =
    let input =
        """
module Domain
enum TrafficLight = Red | Yellow | Green
"""

    assertOfString
        input
        { Name = ComplexName [ "Domain" ]
          Imports = []
          Items =
            [ Enum
                  { Name = ComplexName [ "TrafficLight"; "Domain" ]
                    Symbols = [ "Red"; "Yellow"; "Green" ] } ] }

[<Fact>]
let ``Test record`` () =
    let input =
        """
module Domain
record Crossroad = {
    Id: int
    LongId: long
    AltId: guid
    Street1: string
    Street2: string
    IsMonitored: bool
    Xpos: single
    Ypos: double
    Ratio: money(2)
    LastChecked: timestamp
    ServiceInterval: duration
    CurrentLight: Domain.TrafficLight
    Nickname: option<string>
    Img: bytes
    Notes: array<string>
    Siblings: list<int>
    Props: map<int, string>
}
"""

    assertOfString
        input
        { Name = ComplexName [ "Domain" ]
          Imports = []
          Items =
            [ Record
                  { Name = ComplexName [ "Crossroad"; "Domain" ]
                    IsStruct = false
                    Fields =
                      [ { Name = "Id"
                          Type = Int
                          IsKey = false
                          IsVersion = false
                          Indexes = [] }
                        { Name = "LongId"
                          Type = Long
                          IsKey = false
                          IsVersion = false
                          Indexes = [] }
                        { Name = "AltId"
                          Type = Guid
                          IsKey = false
                          IsVersion = false
                          Indexes = [] }
                        { Name = "Street1"
                          Type = String
                          IsKey = false
                          IsVersion = false
                          Indexes = [] }
                        { Name = "Street2"
                          Type = String
                          IsKey = false
                          IsVersion = false
                          Indexes = [] }
                        { Name = "IsMonitored"
                          Type = Bool
                          IsKey = false
                          IsVersion = false
                          Indexes = [] }
                        { Name = "Xpos"
                          Type = Single
                          IsKey = false
                          IsVersion = false
                          Indexes = [] }
                        { Name = "Ypos"
                          Type = Double
                          IsKey = false
                          IsVersion = false
                          Indexes = [] }
                        { Name = "Ratio"
                          Type = Money 2
                          IsKey = false
                          IsVersion = false
                          Indexes = [] }
                        { Name = "LastChecked"
                          Type = Timestamp
                          IsKey = false
                          IsVersion = false
                          Indexes = [] }
                        { Name = "ServiceInterval"
                          Type = Duration
                          IsKey = false
                          IsVersion = false
                          Indexes = [] }
                        { Name = "CurrentLight"
                          Type = Complex(ComplexName [ "TrafficLight"; "Domain" ])
                          IsKey = false
                          IsVersion = false
                          Indexes = [] }
                        { Name = "Nickname"
                          Type = Optional String
                          IsKey = false
                          IsVersion = false
                          Indexes = [] }
                        { Name = "Img"
                          Type = Bytes
                          IsKey = false
                          IsVersion = false
                          Indexes = [] }
                        { Name = "Notes"
                          Type = Array String
                          IsKey = false
                          IsVersion = false
                          Indexes = [] }
                        { Name = "Siblings"
                          Type = List Int
                          IsKey = false
                          IsVersion = false
                          Indexes = [] }
                        { Name = "Props"
                          Type = Map(Int, String)
                          IsKey = false
                          IsVersion = false
                          Indexes = [] } ] } ] }

[<Fact>]
let ``Test single line record`` () =
    let input =
        """
module Domain
record Crossroad = { Id: int; Street1: string; Street2: string }
"""

    assertOfString
        input
        { Name = ComplexName [ "Domain" ]
          Imports = []
          Items =
            [ Record
                  { Name = ComplexName [ "Crossroad"; "Domain" ]
                    IsStruct = false
                    Fields =
                      [ { Name = "Id"
                          Type = Int
                          IsKey = false
                          IsVersion = false
                          Indexes = [] }
                        { Name = "Street1"
                          Type = String
                          IsKey = false
                          IsVersion = false
                          Indexes = [] }
                        { Name = "Street2"
                          Type = String
                          IsKey = false
                          IsVersion = false
                          Indexes = [] } ] } ] }

[<Fact>]
let ``Test union`` () =
    let input =
        """
module Domain
union ServiceCheck =
    | Random
    | Planned of timestamp
    | Campaign of name:string*step:int
    | RCA of Incident
"""

    assertOfString
        input
        { Name = ComplexName [ "Domain" ]
          Imports = []
          Items =
            [ Union
                  { Name = ComplexName [ "ServiceCheck"; "Domain" ]
                    IsStruct = false
                    Cases =
                      [ { Name = ComplexName [ "Random"; "ServiceCheck"; "Domain" ]
                          IsStruct = false
                          Fields = [] }
                        { Name = ComplexName [ "Planned"; "ServiceCheck"; "Domain" ]
                          IsStruct = false
                          Fields =
                            [ { Name = "p1"
                                Type = Timestamp
                                IsKey = false
                                IsVersion = false
                                Indexes = [] } ] }
                        { Name = ComplexName [ "Campaign"; "ServiceCheck"; "Domain" ]
                          IsStruct = false
                          Fields =
                            [ { Name = "name"
                                Type = String
                                IsKey = false
                                IsVersion = false
                                Indexes = [] }
                              { Name = "step"
                                Type = Int
                                IsKey = false
                                IsVersion = false
                                Indexes = [] } ] }
                        { Name = ComplexName [ "RCA"; "ServiceCheck"; "Domain" ]
                          IsStruct = false
                          Fields =
                            [ { Name = "p1"
                                Type = Complex(ComplexName [ "Incident" ])
                                IsKey = false
                                IsVersion = false
                                Indexes = [] } ] } ] } ] }

[<Fact>]
let ``Test single line union`` () =
    let input =
        """
module Domain
union ServiceCheck = Random | Planned of timestamp | Campaign of name:string*step:int | RCA of Incident
"""

    assertOfString
        input
        { Name = ComplexName [ "Domain" ]
          Imports = []
          Items =
            [ Union
                  { Name = ComplexName [ "ServiceCheck"; "Domain" ]
                    IsStruct = false
                    Cases =
                      [ { Name = ComplexName [ "Random"; "ServiceCheck"; "Domain" ]
                          IsStruct = false
                          Fields = [] }
                        { Name = ComplexName [ "Planned"; "ServiceCheck"; "Domain" ]
                          IsStruct = false
                          Fields =
                            [ { Name = "p1"
                                Type = Timestamp
                                IsKey = false
                                IsVersion = false
                                Indexes = [] } ] }
                        { Name = ComplexName [ "Campaign"; "ServiceCheck"; "Domain" ]
                          IsStruct = false
                          Fields =
                            [ { Name = "name"
                                Type = String
                                IsKey = false
                                IsVersion = false
                                Indexes = [] }
                              { Name = "step"
                                Type = Int
                                IsKey = false
                                IsVersion = false
                                Indexes = [] } ] }
                        { Name = ComplexName [ "RCA"; "ServiceCheck"; "Domain" ]
                          IsStruct = false
                          Fields =
                            [ { Name = "p1"
                                Type = Complex(ComplexName [ "Incident" ])
                                IsKey = false
                                IsVersion = false
                                Indexes = [] } ] } ] } ] }

[<Fact>]
let ``Complex Test`` () =
    let input =
        """
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

    assertOfString
        input
        { Name = ComplexName [ "Foundation"; "Domain" ]
          Imports = []
          Items =
            [ Enum
                  { Name = ComplexName [ "TrafficLight"; "Foundation"; "Domain" ]
                    Symbols = [ "Red"; "Yellow"; "Green" ] }
              Enum
                  { Name = ComplexName [ "AltTrafficLight"; "Foundation"; "Domain" ]
                    Symbols = [ "Red"; "Yellow"; "Blue" ] }
              Record
                  { Name = ComplexName [ "Crossroad"; "Foundation"; "Domain" ]
                    IsStruct = false
                    Fields =
                      [ { Name = "Id"
                          Type = Int
                          IsKey = false
                          IsVersion = false
                          Indexes = [] }
                        { Name = "Street1"
                          Type = String
                          IsKey = false
                          IsVersion = false
                          Indexes = [] }
                        { Name = "Street2"
                          Type = String
                          IsKey = false
                          IsVersion = false
                          Indexes = [] } ] }
              Union
                  { Name = ComplexName [ "ServiceCheck"; "Foundation"; "Domain" ]
                    IsStruct = false
                    Cases =
                      [ { Name = ComplexName [ "Random"; "ServiceCheck"; "Foundation"; "Domain" ]
                          IsStruct = false
                          Fields = [] }
                        { Name = ComplexName [ "Planned"; "ServiceCheck"; "Foundation"; "Domain" ]
                          IsStruct = false
                          Fields =
                            [ { Name = "p1"
                                Type = Timestamp
                                IsKey = false
                                IsVersion = false
                                Indexes = [] } ] }
                        { Name = ComplexName [ "Campaign"; "ServiceCheck"; "Foundation"; "Domain" ]
                          IsStruct = false
                          Fields =
                            [ { Name = "name"
                                Type = String
                                IsKey = false
                                IsVersion = false
                                Indexes = [] }
                              { Name = "step"
                                Type = Int
                                IsKey = false
                                IsVersion = false
                                Indexes = [] } ] }
                        { Name = ComplexName [ "RCA"; "ServiceCheck"; "Foundation"; "Domain" ]
                          IsStruct = false
                          Fields =
                            [ { Name = "p1"
                                Type = Complex(ComplexName [ "Incident" ])
                                IsKey = false
                                IsVersion = false
                                Indexes = [] } ] } ] } ] }

[<Fact>]
let KeyInRecordTest () =
    let input =
        """
module Domain.Foundation
record Crossroad = {
    Id: int key
    Street1: string
    Street2: string key
}
"""

    assertOfString
        input
        { Name = ComplexName [ "Foundation"; "Domain" ]
          Imports = []
          Items =
            [ Record
                  { Name = ComplexName [ "Crossroad"; "Foundation"; "Domain" ]
                    IsStruct = false
                    Fields =
                      [ { Name = "Id"
                          Type = Int
                          IsKey = true
                          IsVersion = false
                          Indexes = [] }
                        { Name = "Street1"
                          Type = String
                          IsKey = false
                          IsVersion = false
                          Indexes = [] }
                        { Name = "Street2"
                          Type = String
                          IsKey = true
                          IsVersion = false
                          Indexes = [] } ] } ] }

[<Fact>]
let KeyInUnionTest () =
    let input =
        """
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

    assertOfString
        input
        { Name = ComplexName [ "Foundation"; "Domain" ]
          Imports = []
          Items =
            [ Record
                  { Name = ComplexName [ "Crossroad"; "Foundation"; "Domain" ]
                    IsStruct = false
                    Fields =
                      [ { Name = "Id"
                          Type = Int
                          IsKey = true
                          IsVersion = false
                          Indexes = [] }
                        { Name = "Street1"
                          Type = String
                          IsKey = false
                          IsVersion = false
                          Indexes = [] }
                        { Name = "Street2"
                          Type = String
                          IsKey = true
                          IsVersion = false
                          Indexes = [] } ] }
              Union
                  { Name = ComplexName [ "U1"; "Foundation"; "Domain" ]
                    IsStruct = false
                    Cases =
                      [ { Name = ComplexName [ "Case1"; "U1"; "Foundation"; "Domain" ]
                          IsStruct = false
                          Fields = [] }
                        { Name = ComplexName [ "Case2"; "U1"; "Foundation"; "Domain" ]
                          IsStruct = false
                          Fields =
                            [ { Name = "p1"
                                Type = Complex(ComplexName [ "Crossroad" ])
                                IsKey = false
                                IsVersion = false
                                Indexes = [] } ] }
                        { Name = ComplexName [ "Case3"; "U1"; "Foundation"; "Domain" ]
                          IsStruct = false
                          Fields =
                            [ { Name = "p1"
                                Type = Complex(ComplexName [ "Crossroad" ])
                                IsKey = true
                                IsVersion = false
                                Indexes = [] } ] }
                        { Name = ComplexName [ "Case4"; "U1"; "Foundation"; "Domain" ]
                          IsStruct = false
                          Fields =
                            [ { Name = "p1"
                                Type = Int
                                IsKey = true
                                IsVersion = false
                                Indexes = [] }
                              { Name = "s1"
                                Type = String
                                IsKey = false
                                IsVersion = false
                                Indexes = [] }
                              { Name = "s2"
                                Type = String
                                IsKey = true
                                IsVersion = false
                                Indexes = [] } ] } ] }

              ] }

[<Fact>]
let IndexInRecordTest () =
    let input =
        """
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

    assertOfString
        input
        { Name = ComplexName [ "Foundation"; "Domain" ]
          Imports = []
          Items =
            [ Record
                  { Name = ComplexName [ "Crossroad"; "Foundation"; "Domain" ]
                    IsStruct = false
                    Fields =
                      [ { Name = "Id"
                          Type = Int
                          IsKey = true
                          IsVersion = false
                          Indexes = [] }
                        { Name = "Street1"
                          Type = String
                          IsKey = false
                          IsVersion = false
                          Indexes =
                            [ { Name = "item"
                                Key = Num
                                Value = Self } ] }
                        { Name = "Street2"
                          Type = String
                          IsKey = true
                          IsVersion = false
                          Indexes =
                            [ { Name = "item"
                                Key = Num
                                Value = Self } ] }
                        { Name = "Street3"
                          Type = String
                          IsKey = false
                          IsVersion = false
                          Indexes =
                            [ { Name = "item"
                                Key = Num
                                Value = Self }
                              { Name = "street"
                                Key = Num
                                Value = Self } ] }
                        { Name = "Street5"
                          Type = String
                          IsKey = false
                          IsVersion = false
                          Indexes =
                            [ { Name = "item"
                                Key = Num
                                Value = IndexValue.Field "Field1" } ] }
                        { Name = "Street6"
                          Type = String
                          IsKey = false
                          IsVersion = false
                          Indexes =
                            [ { Name = "item"
                                Key = IndexKey.FieldKey "Field1"
                                Value = IndexValue.Field "Field2" } ] } ] } ] }


    let input =
        """
module Domain
import "common.protokeep"
enum TrafficLight =
    | Red
    | Yellow
    | Green
"""

    assertOfString
        input
        { Name = ComplexName [ "Domain" ]
          Imports = [ "common.protokeep" ]
          Items =
            [ Enum
                  { Name = ComplexName [ "TrafficLight"; "Domain" ]
                    Symbols = [ "Red"; "Yellow"; "Green" ] } ] }
