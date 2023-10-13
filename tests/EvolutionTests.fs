module EvolutionTests

open Xunit
open Xunit.Sdk
open Protokeep.Types

let assertEqual expected actual =
    if expected <> actual then
        NotEqualException(sprintf "%A" expected, sprintf "%A" actual) |> raise


[<Fact>]
let ``Duplicate Symbols in Enum`` () =
    let input =
        { Name = ComplexName [ "Domain" ]
          Imports = []
          Items =
            [ Enum
                  { Name = ComplexName [ "TrafficLight"; "Domain" ]
                    Type = Int32
                    Symbols = [ "Red"; "Yellow"; "Green"; "Yellow" ] } ] }

    let lock = []

    let expected =
        Error [ Types.DuplicateSymbolsInEnum(ComplexName [ "TrafficLight"; "Domain" ], "Yellow") ]

    assertEqual expected (Types.lock input (LocksCollection lock) (input |> Types.toTypesCacheItems |> Map.ofList))

[<Fact>]
let ``Duplicate Symbols in Locked Enum`` () =
    let input =
        { Name = ComplexName [ "Domain" ]
          Imports = []
          Items =
            [ Enum
                  { Name = ComplexName [ "TrafficLight"; "Domain" ]
                    Type = Int32
                    Symbols = [ "Red"; "Yellow"; "Green" ] } ] }

    let lock =
        [ EnumLock
              { Name = ComplexName [ "TrafficLight"; "Domain" ]
                Type = Int32
                Values =
                  [ { Name = "Red"; Num = 1 }
                    { Name = "Yellow"; Num = 2 }
                    { Name = "Green"; Num = 3 }
                    { Name = "Yellow"; Num = 4 } ] } ]

    let expected =
        Error [ Types.DuplicateSymbolsInLockedEnum(ComplexName [ "TrafficLight"; "Domain" ], "Yellow") ]

    assertEqual expected (Types.lock input (LocksCollection lock) (input |> Types.toTypesCacheItems |> Map.ofList))

[<Fact>]
let ``Missed Symbol in Enum`` () =
    let input =
        { Name = ComplexName [ "Domain" ]
          Imports = []
          Items =
            [ Enum
                  { Name = ComplexName [ "TrafficLight"; "Domain" ]
                    Type = Int32
                    Symbols = [ "Red"; "Yellow"; "Green" ] } ] }

    let lock =
        [ EnumLock
              { Name = ComplexName [ "TrafficLight"; "Domain" ]
                Type = Int32
                Values =
                  [ { Name = "Red"; Num = 1 }
                    { Name = "Yellow"; Num = 2 }
                    { Name = "Green"; Num = 3 }
                    { Name = "Blue"; Num = 4 } ] } ]

    let expected =
        Error [ Types.MissedSymbolInEnum(ComplexName [ "TrafficLight"; "Domain" ], "Blue") ]

    assertEqual expected (Types.lock input (LocksCollection lock) (input |> Types.toTypesCacheItems |> Map.ofList))

[<Fact>]
let ``Single Enum`` () =
    let input =
        { Name = ComplexName [ "Domain" ]
          Imports = []
          Items =
            [ Enum
                  { Name = ComplexName [ "TrafficLight"; "Domain" ]
                    Type = Int32
                    Symbols = [ "Red"; "Yellow"; "Green" ] } ] }

    let lock = []

    let expected =
        Ok
            [ EnumLock
                  { Name = ComplexName [ "TrafficLight"; "Domain" ]
                    Type = Int32
                    Values =
                      [ { Name = "Red"; Num = 1 }
                        { Name = "Yellow"; Num = 2 }
                        { Name = "Green"; Num = 3 } ] } ]

    assertEqual expected (Types.lock input (LocksCollection lock) (input |> Types.toTypesCacheItems |> Map.ofList))

[<Fact>]
let ``Simple Record`` () =
    let input =
        { Name = ComplexName [ "Domain" ]
          Imports = []
          Items =
            [ Record
                  { Name = ComplexName [ "Crossroad"; "Domain" ]
                    IsStruct = false
                    Fields =
                      [ { Name = "Id"
                          Type = Int32
                          IsVersion = false
                          IsKey = false
                          Indexes = [] }
                        { Name = "Street1"
                          Type = String
                          IsVersion = false
                          IsKey = false
                          Indexes = [] }
                        { Name = "Street2"
                          Type = String
                          IsVersion = false
                          IsKey = false
                          Indexes = [] } ] } ] }

    let lock = []

    let expected =
        Ok
            [ RecordLock
                  { Name = ComplexName [ "Crossroad"; "Domain" ]
                    Fields =
                      [ { Name = "Id"; Type = Int32; Num = 1 }
                        { Name = "Street1"
                          Type = String
                          Num = 2 }
                        { Name = "Street2"
                          Type = String
                          Num = 3 } ] } ]

    assertEqual expected (Types.lock input (LocksCollection lock) (input |> Types.toTypesCacheItems |> Map.ofList))


[<Fact>]
let ``Union`` () =
    let input =
        { Name = ComplexName [ "Domain" ]
          Imports = []
          Items =
            [ Record
                  { Name = ComplexName [ "Log"; "Domain" ]
                    IsStruct = false
                    Fields =
                      [ { Name = "Id"
                          Type = Int32
                          IsKey = false
                          IsVersion = false
                          Indexes = [] }
                        { Name = "Check"
                          Type = Complex <| ComplexName [ "ServiceCheck"; "Domain" ]
                          IsKey = false
                          IsVersion = false
                          Indexes = [] } ] }
              Union
                  { Name = ComplexName [ "ServiceCheck"; "Domain" ]
                    IsStruct = false
                    Cases =
                      [ { Name = ComplexName [ "Random"; "ServiceCheck"; "Domain" ]
                          IsStruct = false
                          Fields = [] }
                        { Name = ComplexName [ "Planned"; "ServiceCheck"; "Domain" ]
                          IsStruct = false
                          Fields =
                            [ { Name = "What"
                                Type = String
                                IsKey = false
                                IsVersion = false
                                Indexes = [] }
                              { Name = "Where"
                                Type = String
                                IsKey = false
                                IsVersion = false
                                Indexes = [] }
                              { Name = "When"
                                Type = Timestamp
                                IsKey = false
                                IsVersion = false
                                Indexes = [] } ] } ] } ] }

    let lock = []

    let expected =
        Ok
            [ RecordLock
                  { Name = ComplexName [ "Log"; "Domain" ]
                    Fields =
                      [ { Name = "Id"; Type = Int32; Num = 1 }
                        { Name = "Check"
                          Type = Complex <| ComplexName [ "ServiceCheck"; "Domain" ]
                          Num = 2 } ] }
              UnionLock
                  { Name = ComplexName [ "ServiceCheck"; "Domain" ]
                    Cases = [ { Name = "Random"; Num = 1 }; { Name = "Planned"; Num = 2 } ] }
              RecordLock
                  { Name = ComplexName [ "Random"; "ServiceCheck"; "Domain" ]
                    Fields = [] }
              RecordLock
                  { Name = ComplexName [ "Planned"; "ServiceCheck"; "Domain" ]
                    Fields =
                      [ { Name = "What"
                          Type = String
                          Num = 1 }
                        { Name = "Where"
                          Type = String
                          Num = 2 }
                        { Name = "When"
                          Type = Timestamp
                          Num = 3 } ] } ]

    assertEqual expected (Types.lock input (LocksCollection lock) (input |> Types.toTypesCacheItems |> Map.ofList))

[<Fact>]
let ``Missed Field Record`` () =
    let input =
        { Name = ComplexName [ "Domain" ]
          Imports = []
          Items =
            [ Record
                  { Name = ComplexName [ "Crossroad"; "Domain" ]
                    IsStruct = false
                    Fields =
                      [ { Name = "Id"
                          Type = Int32
                          IsKey = false
                          IsVersion = false
                          Indexes = [] }
                        { Name = "Street1"
                          Type = String
                          IsKey = false
                          IsVersion = false
                          Indexes = [] } ] } ] }

    let lock =
        [ RecordLock
              { Name = ComplexName [ "Crossroad"; "Domain" ]
                Fields =
                  [ { Name = "Id"; Type = Int32; Num = 1 }
                    { Name = "Street1"
                      Type = String
                      Num = 2 }
                    { Name = "Street2"
                      Type = String
                      Num = 3 } ] } ]

    let expected =
        Error [ Types.MissedFieldInRecord(ComplexName [ "Crossroad"; "Domain" ], "Street2") ]

    assertEqual expected (Types.lock input (LocksCollection lock) (input |> Types.toTypesCacheItems |> Map.ofList))


[<Fact>]
let ``Acceptable Evolutionof a Field's Type`` () =
    let input =
        { Name = ComplexName [ "Domain" ]
          Imports = []
          Items =
            [ Record
                  { Name = ComplexName [ "Crossroad"; "Domain" ]
                    IsStruct = false
                    Fields =
                      [ { Name = "Id"
                          Type = Int64
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

    let lock = []

    let expected =
        Ok
            [ RecordLock
                  { Name = ComplexName [ "Crossroad"; "Domain" ]
                    Fields =
                      [ { Name = "Id"; Type = Int64; Num = 1 }
                        { Name = "Street1"
                          Type = String
                          Num = 2 }
                        { Name = "Street2"
                          Type = String
                          Num = 3 } ] } ]

    assertEqual expected (Types.lock input (LocksCollection lock) (input |> Types.toTypesCacheItems |> Map.ofList))

[<Fact>]
let UnacceptableEvolutionOfAFieldType () =
    let input =
        { Name = ComplexName [ "Domain" ]
          Imports = []
          Items =
            [ Record
                  { Name = ComplexName [ "Crossroad"; "Domain" ]
                    IsStruct = false
                    Fields =
                      [ { Name = "Id"
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
                          Indexes = [] } ] } ] }

    let lock =
        [ RecordLock
              { Name = ComplexName [ "Crossroad"; "Domain" ]
                Fields =
                  [ { Name = "Id"; Type = Int32; Num = 1 }
                    { Name = "Street1"
                      Type = String
                      Num = 2 }
                    { Name = "Street2"
                      Type = String
                      Num = 3 } ] } ]

    let expected =
        Error [ Types.UnacceptableEvolution(ComplexName [ "Crossroad"; "Domain" ], "Id", Int32, Guid) ]

    assertEqual expected (Types.lock input (LocksCollection lock) (input |> Types.toTypesCacheItems |> Map.ofList))


[<Fact>]
let MissedFieldInUnion () =
    let input =
        { Name = ComplexName [ "Domain" ]
          Imports = []
          Items =
            [ Record
                  { Name = ComplexName [ "Log"; "Domain" ]
                    IsStruct = false
                    Fields =
                      [ { Name = "Id"
                          Type = Int32
                          IsKey = false
                          IsVersion = false
                          Indexes = [] }
                        { Name = "Check"
                          Type = Complex <| ComplexName [ "ServiceCheck"; "Domain" ]
                          IsKey = false
                          IsVersion = false
                          Indexes = [] } ] }
              ModuleItem.Union
                  { Name = ComplexName [ "ServiceCheck"; "Domain" ]
                    IsStruct = false
                    Cases =
                      [ { Name = ComplexName [ "Random"; "ServiceCheck"; "Domain" ]
                          IsStruct = false
                          Fields = [] } ] } ] }

    let lock =
        [ RecordLock
              { Name = ComplexName [ "Log"; "Domain" ]
                Fields =
                  [ { Name = "Id"; Type = Int32; Num = 1 }
                    { Name = "Check"
                      Type = Complex(ComplexName [ "ServiceCheck"; "Domain" ])
                      Num = 2 } ] }
          UnionLock
              { Name = ComplexName [ "ServiceCheck"; "Domain" ]
                Cases = [ { Name = "Random"; Num = 1 }; { Name = "Planned"; Num = 2 } ] } ]

    let expected =
        Error [ Types.MissedCaseInUnion(ComplexName [ "ServiceCheck"; "Domain" ], "Planned") ]

    assertEqual expected (Types.lock input (LocksCollection lock) (input |> Types.toTypesCacheItems |> Map.ofList))

[<Fact>]
let AddFieldInUnion () =
    let input =
        { Name = ComplexName [ "Domain" ]
          Imports = []
          Items =
            [ Record
                  { Name = ComplexName [ "Log"; "Domain" ]
                    IsStruct = false
                    Fields =
                      [ { Name = "Id"
                          Type = Int32
                          IsKey = false
                          IsVersion = false
                          Indexes = [] }
                        { Name = "Check"
                          Type = Complex <| ComplexName [ "ServiceCheck"; "Domain" ]
                          IsKey = false
                          IsVersion = false
                          Indexes = [] } ] }
              ModuleItem.Union
                  { Name = ComplexName [ "ServiceCheck"; "Domain" ]
                    IsStruct = false
                    Cases =
                      [ { Name = ComplexName [ "Random"; "ServiceCheck"; "Domain" ]
                          IsStruct = false
                          Fields = [] }
                        { Name = ComplexName [ "Planned"; "ServiceCheck"; "Domain" ]
                          IsStruct = false
                          Fields =
                            [ { Name = "What"
                                Type = String
                                IsKey = false
                                IsVersion = false
                                Indexes = [] }
                              { Name = "Where"
                                Type = String
                                IsKey = false
                                IsVersion = false
                                Indexes = [] }
                              { Name = "When"
                                Type = Timestamp
                                IsKey = false
                                IsVersion = false
                                Indexes = [] } ] }
                        { Name = ComplexName [ "NewCase"; "ServiceCheck"; "Domain" ]
                          IsStruct = false
                          Fields = [] } ] } ] }

    let lock = []

    let expected =
        Ok
            [ RecordLock
                  { Name = ComplexName [ "Log"; "Domain" ]
                    Fields =
                      [ { Name = "Id"; Type = Int32; Num = 1 }
                        { Name = "Check"
                          Type = Complex(ComplexName [ "ServiceCheck"; "Domain" ])
                          Num = 2 } ] }
              UnionLock
                  { Name = ComplexName [ "ServiceCheck"; "Domain" ]
                    Cases =
                      [ { Name = "Random"; Num = 1 }
                        { Name = "Planned"; Num = 2 }
                        { Name = "NewCase"; Num = 3 } ] }
              RecordLock
                  { Name = ComplexName [ "Random"; "ServiceCheck"; "Domain" ]
                    Fields = [] }
              RecordLock
                  { Name = ComplexName [ "Planned"; "ServiceCheck"; "Domain" ]
                    Fields =
                      [ { Name = "What"
                          Type = String
                          Num = 1 }
                        { Name = "Where"
                          Type = String
                          Num = 2 }
                        { Name = "When"
                          Type = Timestamp
                          Num = 3 } ] }
              RecordLock
                  { Name = ComplexName [ "NewCase"; "ServiceCheck"; "Domain" ]
                    Fields = [] } ]

    assertEqual expected (Types.lock input (LocksCollection lock) (input |> Types.toTypesCacheItems |> Map.ofList))
