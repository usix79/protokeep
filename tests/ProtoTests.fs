module ProtoTests

open System
open FSharp.Reflection
open Xunit
open Protogen.Types
open Protogen

type TestData() =
  static member MyTestData =
    [
        ([], [], "syntax = \"proto3\";")
        ([{
            Name = ComplexName ["Domain"];
            Items = [
                Enum {
                    Name = "TrafficLight"
                    Symbols = ["Red"; "Yellow"; "Green"]
                }
            ]
           }],
         [EnumLock {
                Name = ComplexName ["TrafficLight"; "Domain"]
                Values = [
                    {Name = "Red"; Num = 1}
                    {Name = "Yellow"; Num = 2}
                    {Name = "Green"; Num = 3}
                ]}
         ],
         """
syntax = "proto3";
package Domain;
enum TrafficLight {
    Unknown = 0;
    Red = 1;
    Yellow = 2;
    Green = 3;
}
""")
    ] |> Seq.map FSharpValue.GetTupleFields

[<Theory; MemberData("MyTestData", MemberType=typeof<TestData>)>]
let testAllCases (modules, locks, expectedOutput:string) =
    Assert.Equal(expectedOutput.Trim(), (ProtoCmd.gen modules locks).Trim())
