module EvolutionTests

open System
open Xunit
open Xunit.Sdk
open Protogen.Types

let assertEqual expected actual =
    if expected <> actual then
        NotEqualException (sprintf "%A" expected, sprintf "%A" actual) |> raise


[<Fact>]
let ``Duplicate Symbols in Enum`` () =
    let input = {Name = ComplexName ["Domain"]; Items = [
        Enum {Name = ComplexName ["TrafficLight"; "Domain"]; Symbols = ["Red"; "Yellow"; "Green"; "Yellow"]}
        ]}
    let lock = []
    let expected = Error [ Types.DuplicateSymbolsInEnum (ComplexName ["TrafficLight"; "Domain"], "Yellow")]

    assertEqual expected (Types.lock input (LocksCollection lock) (input |> Types.toTypesCacheItems |> Map.ofList))

[<Fact>]
let ``Duplicate Symbols in Locked Enum`` () =
    let input = {Name = ComplexName ["Domain"]; Items = [
        Enum {Name = ComplexName ["TrafficLight"; "Domain"]; Symbols = ["Red"; "Yellow"; "Green"]}
        ]}
    let lock = [
        EnumLock {
            Name = ComplexName ["TrafficLight"; "Domain"]
            Values = [
                {Name = "Red"; Num = 1}
                {Name = "Yellow"; Num = 2}
                {Name = "Green"; Num = 3}
                {Name = "Yellow"; Num = 4}
            ]}]
    let expected = Error [ Types.DuplicateSymbolsInLockedEnum (ComplexName ["TrafficLight"; "Domain"], "Yellow")]

    assertEqual expected (Types.lock input (LocksCollection lock) (input |> Types.toTypesCacheItems |> Map.ofList))

[<Fact>]
let ``Missed Symbol in Enum`` () =
    let input = {Name = ComplexName ["Domain"]; Items = [
        Enum {Name = ComplexName ["TrafficLight"; "Domain"]; Symbols = ["Red"; "Yellow"; "Green"]}
        ]}
    let lock = [
        EnumLock {
            Name = ComplexName ["TrafficLight"; "Domain"]
            Values = [
                {Name = "Red"; Num = 1}
                {Name = "Yellow"; Num = 2}
                {Name = "Green"; Num = 3}
                {Name = "Blue"; Num = 4}
            ]}]
    let expected = Error [ Types.MissedSymbolInEnum (ComplexName ["TrafficLight"; "Domain"], "Blue")]

    assertEqual expected (Types.lock input (LocksCollection lock) (input |> Types.toTypesCacheItems |> Map.ofList))

[<Fact>]
let ``Single Enum`` () =
    let input = {Name = ComplexName ["Domain"]; Items = [Enum {Name = ComplexName ["TrafficLight"; "Domain"]; Symbols = ["Red"; "Yellow"; "Green"]}]}
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

    assertEqual expected (Types.lock input (LocksCollection lock) (input |> Types.toTypesCacheItems |> Map.ofList))

[<Fact>]
let ``Simple Record`` () =
    let input = {
        Name = ComplexName ["Domain"]
        Items = [
            Record {Name = ComplexName ["Crossroad"; "Domain"]; Fields = [
                {Name= "Id"; Type = Int; IsKey = false}
                {Name= "Street1"; Type = String; IsKey = false}
                {Name= "Street2"; Type = String; IsKey = false}
                ]}
        ]}
    let lock = []
    let expected = Ok [
        RecordLock {
            Name= ComplexName ["Crossroad"; "Domain"]
            Fields = [
                {Name = "Id"; Type = Int; IsKey = false; Num = 1}
                {Name = "Street1"; Type = String; IsKey = false; Num = 2}
                {Name = "Street2"; Type = String; IsKey = false; Num = 3}
            ]}
        MessageLock {
            Name= ComplexName ["Crossroad"; "Domain"]
            LockItems = [
                Field {Name = "Id"; Type = Int; Num = 1}
                Field {Name = "Street1"; Type = String; Num = 2}
                Field {Name = "Street2"; Type = String; Num = 3}
            ]}
        ]

    assertEqual expected (Types.lock input (LocksCollection lock) (input |> Types.toTypesCacheItems |> Map.ofList))


[<Fact>]
let ``Union`` () =
    let input = {
        Name = ComplexName ["Domain"]
        Items = [
            Record {Name = ComplexName ["Log"; "Domain"]; Fields = [
                {Name= "Id"; Type = Int; IsKey = false}
                {Name= "Check"; Type = Complex <| ComplexName ["ServiceCheck"; "Domain"]; IsKey = false}
                ]}
            Union {Name = ComplexName ["ServiceCheck"; "Domain"]; Cases = [
                {Name = ComplexName ["Random"; "ServiceCheck"; "Domain"]; Fields = []}
                {Name = ComplexName ["Planned"; "ServiceCheck"; "Domain"]; Fields = [
                    {Name = "What"; Type = String; IsKey = false}
                    {Name = "Where"; Type = String; IsKey = false}
                    {Name = "When"; Type = Timestamp; IsKey = false}
                    ]}
            ]}
        ]}
    let lock = []
    let expected = Ok [
        RecordLock {
            Name = ComplexName ["Log"; "Domain"]
            Fields = [
                {Name = "Id"; Type = Int; IsKey = false; Num = 1}
                {Name = "Check"; Type = Complex <| ComplexName ["ServiceCheck"; "Domain"]; IsKey = false; Num = 2}
            ]}
        MessageLock {
            Name = ComplexName ["Log"; "Domain"]
            LockItems = [
                Field {Name = "Id"; Type = Int; Num = 1}
                OneOf ("Check",ComplexName ["ServiceCheck"; "Domain"],[
                    {CaseName = "Random"; Num = 2}
                    {CaseName = "Planned"; Num = 3}
                ])
            ]}
        UnionLock {
            Name = ComplexName ["ServiceCheck"; "Domain"]
            Cases = [
                { Name = "Random"; Num = 1 }
                { Name = "Planned"; Num = 2 }
            ]};
        RecordLock { Name = ComplexName ["Random"; "ServiceCheck"; "Domain"]; Fields = [] };
        RecordLock {
           Name = ComplexName ["Planned"; "ServiceCheck"; "Domain"]
           Fields = [
                    { Name = "What"; Type = String; Num = 1; IsKey = false }
                    { Name = "Where"; Type = String; Num = 2; IsKey = false }
                    { Name = "When"; Type = Timestamp; Num = 3; IsKey = false }
                    ]};
        MessageLock {
            Name = ComplexName ["Random"; "ServiceCheck"; "Domain"]
            LockItems = [] }
        MessageLock {
            Name = ComplexName ["Planned"; "ServiceCheck"; "Domain"]
            LockItems = [
                Field { Name = "What"; Type = String; Num = 1 };
                Field { Name = "Where"; Type = String;  Num = 2 };
                Field { Name = "When"; Type = Timestamp; Num = 3 }] }
        ]

    assertEqual expected (Types.lock input (LocksCollection lock) (input |> Types.toTypesCacheItems |> Map.ofList))

[<Fact>]
let ``Missed Field Record`` () =
    let input = {
        Name = ComplexName ["Domain"]
        Items = [
            Record {Name = ComplexName ["Crossroad"; "Domain"]; Fields = [
                {Name= "Id"; Type = Int; IsKey = false}
                {Name= "Street1"; Type = String; IsKey = false}
                ]}
        ]}
    let lock = [
        RecordLock {
            Name = ComplexName ["Crossroad"; "Domain"];
            Fields = [{ Name = "Id"
                        Type = Int
                        Num = 1
                        IsKey = false }; { Name = "Street1"
                                           Type = String
                                           Num = 2
                                           IsKey = false }; { Name = "Street2"
                                                              Type = String
                                                              Num = 3
                                                              IsKey = false }] };
        MessageLock {
            Name= ComplexName ["Crossroad"; "Domain"]
            LockItems = [
                Field {Name = "Id"; Type = Int; Num = 1}
                Field {Name = "Street1"; Type = String; Num = 2}
                Field {Name = "Street2"; Type = String; Num = 3}
            ]}
        ]

    let expected = Error [ Types.MissedFieldInRecord (ComplexName ["Crossroad"; "Domain"], "Street2") ]

    assertEqual expected (Types.lock input (LocksCollection lock) (input |> Types.toTypesCacheItems |> Map.ofList))


[<Fact>]
let ``Acceptable Evolutionof a Field's Type`` () =
    let input = {
        Name = ComplexName ["Domain"]
        Items = [
            Record {Name = ComplexName ["Crossroad"; "Domain"]; Fields = [
                {Name= "Id"; Type = Long; IsKey = false}
                {Name= "Street1"; Type = String; IsKey = false}
                {Name= "Street2"; Type = String; IsKey = false}
                ]}
        ]}
    let lock = [
        MessageLock {
            Name= ComplexName ["Crossroad"; "Domain"]
            LockItems = [
                Field {Name = "Id"; Type = Int; Num = 1}
                Field {Name = "Street1"; Type = String; Num = 2}
                Field {Name = "Street2"; Type = String; Num = 3}
            ]}
        ]

    let expected = Ok [
          RecordLock
             { Name = ComplexName ["Crossroad"; "Domain"]
               Fields = [{ Name = "Id"
                           Type = Long
                           Num = 1
                           IsKey = false }; { Name = "Street1"
                                              Type = String
                                              Num = 2
                                              IsKey = false }; { Name = "Street2"
                                                                 Type = String
                                                                 Num = 3
                                                                 IsKey = false }] };
          MessageLock {
            Name= ComplexName ["Crossroad"; "Domain"]
            LockItems = [
                Field {Name = "Id"; Type = Long; Num = 1}
                Field {Name = "Street1"; Type = String; Num = 2}
                Field {Name = "Street2"; Type = String; Num = 3}
            ]}
        ]
    assertEqual expected (Types.lock input (LocksCollection lock) (input |> Types.toTypesCacheItems |> Map.ofList))

[<Fact>]
let UnacceptableEvolutionOfAFieldType () =
    let input = {
        Name = ComplexName ["Domain"]
        Items = [
            Record {Name = ComplexName ["Crossroad"; "Domain"]; Fields = [
                {Name= "Id"; Type = Guid; IsKey = false}
                {Name= "Street1"; Type = String; IsKey = false}
                {Name= "Street2"; Type = String; IsKey = false}
                ]}
        ]}
    let lock = [
        RecordLock
             { Name = ComplexName ["Crossroad"; "Domain"]
               Fields = [{ Name = "Id"
                           Type = Int
                           Num = 1
                           IsKey = false }; { Name = "Street1"
                                              Type = String
                                              Num = 2
                                              IsKey = false }; { Name = "Street2"
                                                                 Type = String
                                                                 Num = 3
                                                                 IsKey = false }] };
        MessageLock {
            Name= ComplexName ["Crossroad"; "Domain"]
            LockItems = [
                Field {Name = "Id"; Type = Int; Num = 1}
                Field {Name = "Street1"; Type = String; Num = 2}
                Field {Name = "Street2"; Type = String; Num = 3}
            ]}
        ]

    let expected = Error [ Types.UnacceptableEvolution(ComplexName ["Crossroad"; "Domain"], "Id", Int, Guid)]

    assertEqual expected (Types.lock input (LocksCollection lock) (input |> Types.toTypesCacheItems |> Map.ofList))


[<Fact>]
let MissedFieldInUnion() =
    let input = {
        Name = ComplexName ["Domain"]
        Items = [
            Record {Name = ComplexName ["Log"; "Domain"]; Fields = [
                {Name= "Id"; Type = Int; IsKey = false}
                {Name= "Check"; Type = Complex <| ComplexName ["ServiceCheck"; "Domain"]; IsKey = false}
                ]}
            ModuleItem.Union {Name = ComplexName ["ServiceCheck"; "Domain"]; Cases = [
                {Name = ComplexName ["Random"; "ServiceCheck"; "Domain"]; Fields = []}
            ]}
        ]}
    let lock = [
        RecordLock
             { Name = ComplexName ["Log"; "Domain"]
               Fields =
                       [{ Name = "Id"
                          Type = Int
                          Num = 1
                          IsKey = false };
                        { Name = "Check"
                          Type = Complex (ComplexName ["ServiceCheck"; "Domain"])
                          Num = 2
                          IsKey = false }] };
        MessageLock {
            Name = ComplexName ["Log"; "Domain"]
            LockItems = [
                Field {Name = "Id"; Type = Int; Num = 1}
                OneOf ("Check",ComplexName ["ServiceCheck"; "Domain"],[
                    {CaseName = "Random"; Num = 2}
                    {CaseName = "Planned"; Num = 3}
                ])
            ]}
        UnionLock {
            Name = ComplexName ["ServiceCheck"; "Domain"]
            Cases = [
                { Name = "Random"; Num = 1 }
                { Name = "Planned"; Num = 2 }
            ] };
        MessageLock {
            Name = ComplexName ["Random"; "ServiceCheck"; "Domain"]
            LockItems = [] }
        MessageLock {
            Name = ComplexName ["Planned"; "ServiceCheck"; "Domain"]
            LockItems = [
                Field { Name = "What"; Type = String; Num = 1 };
                Field { Name = "Where"; Type = String;  Num = 2 };
                Field { Name = "When"; Type = Timestamp; Num = 3 }] }
        ]

    let expected = Error [ Types.MissedCaseInUnion (ComplexName ["ServiceCheck"; "Domain"], "Planned")]

    assertEqual expected (Types.lock input (LocksCollection lock) (input |> Types.toTypesCacheItems |> Map.ofList))

[<Fact>]
let AddFieldInUnion() =
    let input = {
        Name = ComplexName ["Domain"]
        Items = [
            Record {Name = ComplexName ["Log"; "Domain"]; Fields = [
                {Name= "Id"; Type = Int; IsKey = false}
                {Name= "Check"; Type = Complex <| ComplexName ["ServiceCheck"; "Domain"]; IsKey = false}
                ]}
            ModuleItem.Union {Name = ComplexName ["ServiceCheck"; "Domain"]; Cases = [
                {Name = ComplexName ["Random"; "ServiceCheck"; "Domain"]; Fields = []}
                {Name = ComplexName ["Planned"; "ServiceCheck"; "Domain"]; Fields = [
                    { Name = "What"; Type = String; IsKey = false};
                    { Name = "Where"; Type = String; IsKey = false};
                    { Name = "When"; Type = Timestamp; IsKey = false}
                ]}
                {Name = ComplexName ["NewCase"; "ServiceCheck"; "Domain"]; Fields = []}
            ]}
        ]}
    let lock = [
        MessageLock {
            Name = ComplexName ["Log"; "Domain"]
            LockItems = [
                Field {Name = "Id"; Type = Int; Num = 1}
                OneOf ("Check",ComplexName ["ServiceCheck"; "Domain"],[
                    {CaseName = "Random"; Num = 2}
                    {CaseName = "Planned"; Num = 3}
                ])
            ]}
        MessageLock {
            Name = ComplexName ["Random"; "ServiceCheck"; "Domain"]
            LockItems = [] }
        MessageLock {
            Name = ComplexName ["Planned"; "ServiceCheck"; "Domain"]
            LockItems = [
                Field { Name = "What"; Type = String; Num = 1 }
                Field { Name = "Where"; Type = String;  Num = 2 }
                Field { Name = "When"; Type = Timestamp; Num = 3 }] }
        ]

    let expected = Ok [
           RecordLock
             { Name = ComplexName ["Log"; "Domain"]
               Fields =
                       [{ Name = "Id"
                          Type = Int
                          Num = 1
                          IsKey = false };
                        { Name = "Check"
                          Type = Complex (ComplexName ["ServiceCheck"; "Domain"])
                          Num = 2
                          IsKey = false }] };
           MessageLock
             { Name = ComplexName ["Log"; "Domain"]
               LockItems =
                          [Field { Name = "Id"
                                   Type = Int
                                   Num = 1 };
                           OneOf
                             ("Check", ComplexName ["ServiceCheck"; "Domain"],
                              [{ CaseName = "Random"
                                 Num = 2 }; { CaseName = "Planned"
                                              Num = 3 }; { CaseName = "NewCase"
                                                           Num = 4 }])] };
           UnionLock { Name = ComplexName ["ServiceCheck"; "Domain"]
                       Cases = [{ Name = "Random"
                                  Num = 1 }; { Name = "Planned"
                                               Num = 2 }; { Name = "NewCase"
                                                            Num = 3 }] };
           RecordLock { Name = ComplexName ["Random"; "ServiceCheck"; "Domain"]
                        Fields = [] };
           RecordLock
             { Name = ComplexName ["Planned"; "ServiceCheck"; "Domain"]
               Fields = [{ Name = "What"
                           Type = String
                           Num = 1
                           IsKey = false }; { Name = "Where"
                                              Type = String
                                              Num = 2
                                              IsKey = false }; { Name = "When"
                                                                 Type = Timestamp
                                                                 Num = 3
                                                                 IsKey = false }] };
           RecordLock { Name = ComplexName ["NewCase"; "ServiceCheck"; "Domain"]
                        Fields = [] };
           MessageLock { Name = ComplexName ["Random"; "ServiceCheck"; "Domain"]
                         LockItems = [] };
           MessageLock
             { Name = ComplexName ["Planned"; "ServiceCheck"; "Domain"]
               LockItems =
                          [Field { Name = "What"
                                   Type = String
                                   Num = 1 }; Field { Name = "Where"
                                                      Type = String
                                                      Num = 2 };
                           Field { Name = "When"
                                   Type = Timestamp
                                   Num = 3 }] };
           MessageLock { Name = ComplexName ["NewCase"; "ServiceCheck"; "Domain"]
                         LockItems = [] }]

    assertEqual expected (Types.lock input (LocksCollection lock) (input |> Types.toTypesCacheItems |> Map.ofList))
