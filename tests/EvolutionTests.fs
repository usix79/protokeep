module EvolutionTests

open System
open Xunit
open Xunit.Sdk
open Protogen.Types

let assertEqual expected actual =
    if expected <> actual then
        NotEqualException (sprintf "%A" expected, sprintf "%A" actual) |> raise

[<Fact>]
let ``Duplicate Enums`` () =
    let input = {Name = ComplexName ["Domain"]; Items = [
        Enum {Name = "TrafficLight"; Symbols = ["Red"; "Yellow"; "Green"]}
        Enum {Name = "TrafficLight"; Symbols = ["Red"; "Yellow"; "Green"]}]}
    let lock = []
    let expected = Error [ Types.DuplicateTypeNames (ComplexName ["TrafficLight"; "Domain"])]

    assertEqual expected (Types.lock input lock)

[<Fact>]
let ``Duplicate Locked Enums`` () =
    let input = {Name = ComplexName ["Domain"]; Items = [
        Enum {Name = "TrafficLight"; Symbols = ["Red"; "Yellow"; "Green"; "Yellow"]}
        ]}
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
    let expected = Error [ Types.DuplicateLockedTypeNames (ComplexName ["TrafficLight"; "Domain"])]

    assertEqual expected (Types.lock input lock)

[<Fact>]
let ``Duplicate Symbols in Enum`` () =
    let input = {Name = ComplexName ["Domain"]; Items = [
        Enum {Name = "TrafficLight"; Symbols = ["Red"; "Yellow"; "Green"; "Yellow"]}
        ]}
    let lock = []
    let expected = Error [ Types.DuplicateSymbolsInEnum (ComplexName ["TrafficLight"; "Domain"], "Yellow")]

    assertEqual expected (Types.lock input lock)

[<Fact>]
let ``Duplicate Symbols in Locked Enum`` () =
    let input = {Name = ComplexName ["Domain"]; Items = [
        Enum {Name = "TrafficLight"; Symbols = ["Red"; "Yellow"; "Green"]}
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

    assertEqual expected (Types.lock input lock)

[<Fact>]
let ``Missed Symbol in Enum`` () =
    let input = {Name = ComplexName ["Domain"]; Items = [
        Enum {Name = "TrafficLight"; Symbols = ["Red"; "Yellow"; "Green"]}
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

    assertEqual expected (Types.lock input lock)

[<Fact>]
let ``Single Enum`` () =
    let input = {Name = ComplexName ["Domain"]; Items = [Enum {Name = "TrafficLight"; Symbols = ["Red"; "Yellow"; "Green"]}]}
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

    assertEqual expected (Types.lock input lock)

[<Fact>]
let ``Unknown type of record's field`` () =
    let input = {
        Name = ComplexName ["Domain"]
        Items = [
            Record {Name = "Simple"; Fields = [{Name= "Id"; Type = Complex (ComplexName ["XXX"])}]}
        ]}
    let lock = []
    let expected = Error [ Types.UnknownFieldType (ComplexName ["Simple"; "Domain"], "Id", ComplexName ["XXX"])]

    assertEqual expected (Types.lock input lock)

[<Fact>]
let ``Simple Record`` () =
    let input = {
        Name = ComplexName ["Domain"]
        Items = [
            Record {Name = "Crossroad"; Fields = [
                {Name= "Id"; Type = Int}
                {Name= "Street1"; Type = String}
                {Name= "Street2"; Type = String}
                ]}
        ]}
    let lock = []
    let expected = Ok [
        MessageLock {
            Name= ComplexName ["Crossroad"; "Domain"]
            LockItems = [
                Field {Name = "Id"; Type = Int; Num = 1}
                Field {Name = "Street1"; Type = String; Num = 2}
                Field {Name = "Street2"; Type = String; Num = 3}
            ]}
        ]

    assertEqual expected (Types.lock input lock)


[<Fact>]
let ``Union`` () =
    let input = {
        Name = ComplexName ["Domain"]
        Items = [
            Record {Name = "Log"; Fields = [
                {Name= "Id"; Type = Int}
                {Name= "Check"; Type = Complex <| ComplexName ["ServiceCheck"; "Domain"]}
                ]}
            Union {Name = "ServiceCheck"; Cases = [
                {Name = "Random"; Fields = []}
                {Name = "Planned"; Fields = [
                    {Name = "What"; Type = String}
                    {Name = "Where"; Type = String}
                    {Name = "When"; Type = Timestamp}
                    ]}
            ]}
        ]}
    let lock = []
    let expected = Ok [
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
                Field { Name = "What"; Type = String; Num = 1 };
                Field { Name = "Where"; Type = String;  Num = 2 };
                Field { Name = "When"; Type = Timestamp; Num = 3 }] }
        ]

    assertEqual expected (Types.lock input lock)

[<Fact>]
let ``Missed Field Record`` () =
    let input = {
        Name = ComplexName ["Domain"]
        Items = [
            Record {Name = "Crossroad"; Fields = [
                {Name= "Id"; Type = Int}
                {Name= "Street1"; Type = String}
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

    let expected = Error [ Types.MissedFieldInRecord (ComplexName ["Crossroad"; "Domain"], "Street2") ]

    assertEqual expected (Types.lock input lock)


[<Fact>]
let ``Acceptable Evolutionof a Field's Type`` () =
    let input = {
        Name = ComplexName ["Domain"]
        Items = [
            Record {Name = "Crossroad"; Fields = [
                {Name= "Id"; Type = Long}
                {Name= "Street1"; Type = String}
                {Name= "Street2"; Type = String}
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
        MessageLock {
            Name= ComplexName ["Crossroad"; "Domain"]
            LockItems = [
                Field {Name = "Id"; Type = Long; Num = 1}
                Field {Name = "Street1"; Type = String; Num = 2}
                Field {Name = "Street2"; Type = String; Num = 3}
            ]}
        ]
    assertEqual expected (Types.lock input lock)

[<Fact>]
let UnacceptableEvolutionOfAFieldType () =
    let input = {
        Name = ComplexName ["Domain"]
        Items = [
            Record {Name = "Crossroad"; Fields = [
                {Name= "Id"; Type = Guid}
                {Name= "Street1"; Type = String}
                {Name= "Street2"; Type = String}
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

    let expected = Error [ Types.UnacceptableEvolution(ComplexName ["Crossroad"; "Domain"], "Id", Int, Guid)]

    assertEqual expected (Types.lock input lock)


[<Fact>]
let MissedFieldInUnion() =
    let input = {
        Name = ComplexName ["Domain"]
        Items = [
            Record {Name = "Log"; Fields = [
                {Name= "Id"; Type = Int}
                {Name= "Check"; Type = Complex <| ComplexName ["ServiceCheck"; "Domain"]}
                ]}
            ModuleItem.Union {Name = "ServiceCheck"; Cases = [
                {Name = "Random"; Fields = []}
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
                Field { Name = "What"; Type = String; Num = 1 };
                Field { Name = "Where"; Type = String;  Num = 2 };
                Field { Name = "When"; Type = Timestamp; Num = 3 }] }
        ]

    let expected = Error [ Types.MissedCaseInUnion (ComplexName ["Log"; "Domain"], ComplexName ["ServiceCheck"; "Domain"], "Planned")]

    assertEqual expected (Types.lock input lock)

[<Fact>]
let AddFieldInUnion() =
    let input = {
        Name = ComplexName ["Domain"]
        Items = [
            Record {Name = "Log"; Fields = [
                {Name= "Id"; Type = Int}
                {Name= "Check"; Type = Complex <| ComplexName ["ServiceCheck"; "Domain"]}
                ]}
            ModuleItem.Union {Name = "ServiceCheck"; Cases = [
                {Name = "Random"; Fields = []}
                {Name = "Planned"; Fields = [
                    { Name = "What"; Type = String};
                    { Name = "Where"; Type = String};
                    { Name = "When"; Type = Timestamp}
                ]}
                {Name = "NewCase"; Fields = []}
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
        MessageLock {
            Name = ComplexName ["Log"; "Domain"]
            LockItems = [
                Field {Name = "Id"; Type = Int; Num = 1}
                OneOf ("Check",ComplexName ["ServiceCheck"; "Domain"],[
                    {CaseName = "Random"; Num = 2}
                    {CaseName = "Planned"; Num = 3}
                    {CaseName = "NewCase"; Num = 4}
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
        MessageLock {
            Name = ComplexName ["NewCase"; "ServiceCheck"; "Domain"]
            LockItems = [] }
        ]


    assertEqual expected (Types.lock input lock)
