module LockDocTests

open System
open FSharp.Reflection
open Xunit
open Protogen.Types
open Protogen

let private assertOfString input expected =
    match Parsers.parseLockDoc input with
    | Ok res -> Assert.Equal<LockItem list>(expected, res)
    | Error err -> failwith err

type TestData() =
  static member MyTestData =
    [
        ([], "");
        ([
            EnumLock {
                Name = ComplexName ["TrafficLight"; "Domain"]
                Values = [
                    {Name = "Red"; Num = 1}
                    {Name = "Yellow"; Num = 2}
                    {Name = "Green"; Num = 3}
                ]}
            ],"""enum Domain.TrafficLight
    value Red = 1
    value Yellow = 2
    value Green = 3
""");
        ([
        MessageLock {
            Name= ComplexName ["Crossroad"; "Domain"]
            LockItems = [
                Field {Name = "Id"; Type = Int; Num = 1}
                Field {Name = "Street1"; Type = String; Num = 2}
                Field {Name = "Street2"; Type = String; Num = 3}
            ]}
        ], """message Domain.Crossroad
    field Id int = 1
    field Street1 string = 2
    field Street2 string = 3
""");
        ([
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
                Field { Name = "When"; Type = Timespamp; Num = 3 }] }
        ], """message Domain.Log
    field Id int = 1
    oneof Check Domain.ServiceCheck
        case Random = 2
        case Planned = 3
message Domain.ServiceCheck.Random
message Domain.ServiceCheck.Planned
    field What string = 1
    field Where string = 2
    field When timestamp = 3
""")
    ] |> Seq.map FSharpValue.GetTupleFields


[<Theory; MemberData("MyTestData", MemberType=typeof<TestData>)>]
let testAllCases (locks, txt) =
    Assert.Equal(txt, LockFile.gen locks)
    assertOfString txt locks