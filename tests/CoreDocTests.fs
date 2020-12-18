module CoreDocTests

open FSharp.Reflection
open Xunit
open Protogen.Types
open Protogen

type TestData() =
  static member FsharpTestData =
    [
        ("""""", "Test", """
    any thing
        you may im agine
""", """
namespace Protogen
module Test =

    any thing
        you may im agine
""");
        ("""namespace Protogen
module Test =
    old data
    old ddd
module XXX =
    1,3,4
    45
""", "Test", """
    any thing
        you may imagine
""", """namespace Protogen
module Test =

    any thing
        you may imagine

module XXX =
    1,3,4
    45
""")
    ] |> Seq.map FSharpValue.GetTupleFields

[<Theory; MemberData("FsharpTestData", MemberType=typeof<TestData>)>]
let testAllCases (coreTxt:string, moduleName:string, moduleTxt:string, expectedOutput:string) =
    let outputText = Codegen.CoreFsharp.update coreTxt moduleName moduleTxt
    Assert.Equal(expectedOutput.Trim(), outputText.Trim())
