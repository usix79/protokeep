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
        RecordLock {
            Name= ComplexName ["Crossroad"; "Domain"]
            Fields = [
                {Name = "Id"; Type = Int; Num = 1}
                {Name = "Street1"; Type = String; Num = 2}
                {Name = "Street2"; Type = String; Num = 3}
            ]}
        ], """record Domain.Crossroad
    field Id int = 1
    field Street1 string = 2
    field Street2 string = 3
""");
    ] |> Seq.map FSharpValue.GetTupleFields


[<Theory; MemberData("MyTestData", MemberType=typeof<TestData>)>]
let testAllCases (locks, txt) =
    Assert.Equal(txt, LockCmd.gen locks)
    assertOfString txt locks