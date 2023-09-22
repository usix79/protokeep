module CoreDocTests

open Xunit
open Protokeep

type TestCase =
    { CoreText: string
      ModuleName: string
      ModuleText: string
      ExpectedOutput: string }

let testCases =
    [ { CoreText = ""
        ModuleName = "Test"
        ModuleText =
          "
    anything
        you may imagine
"
        ExpectedOutput =
          "
namespace Protokeep
module Test =

    anything
        you may imagine
"     }
      { CoreText =
          "namespace Protokeep
module Test =
    old data
    old ddd
module XXX =
    1,3,4
    45"
        ModuleName = "Test"
        ModuleText =
          "
    anything
        you may imagine
"
        ExpectedOutput =
          "
namespace Protokeep
module Test =

    anything
        you may imagine

module XXX =
    1,3,4
    45
"     } ]

[<Fact>]
let ``All Core Test Cases Should Pass`` () =
    for testCase in testCases do
        let coreTxt = testCase.CoreText
        let moduleName = testCase.ModuleName
        let moduleTxt = testCase.ModuleText
        let expectedOutput = testCase.ExpectedOutput

        let outputText = Codegen.CoreFsharp.update coreTxt moduleName moduleTxt
        Assert.Equal(expectedOutput.Trim(), outputText.Trim())
