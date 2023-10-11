module LockDocTests

open Xunit
open Protokeep.Types
open Protokeep

let private assertOfString input expected =
    match Parsers.parseLockDoc input with
    | Ok res -> Assert.Equal<LockItem list>(expected, res)
    | Error err -> failwith err

type TestCase =
    { Locks: LockItem list
      ExpectedOutput: string }

let testCases =
    [ { Locks = []; ExpectedOutput = "" }
      { Locks =
          [ EnumLock
                { Name = ComplexName [ "TrafficLight"; "Domain" ]
                  Values =
                    [ { Name = "Red"; Num = 1 }
                      { Name = "Yellow"; Num = 2 }
                      { Name = "Green"; Num = 3 } ] } ]
        ExpectedOutput =
          "enum Domain.TrafficLight
    value Red = 1
    value Yellow = 2
    value Green = 3
"     }
      { Locks =
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
        ExpectedOutput =
          "record Domain.Crossroad
    field Id int32 = 1
    field Street1 string = 2
    field Street2 string = 3
"     } ]

let ``All lock test cases should pass`` () =
    for testCase in testCases do
        let locks = testCase.Locks
        let txt = testCase.ExpectedOutput
        Assert.Equal(txt, LockCmd.gen locks)
        assertOfString txt locks
