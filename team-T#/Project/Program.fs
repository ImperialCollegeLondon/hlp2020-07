open TopModule
open TestsModule
open System
open Expecto

let makeMyTests (x,y,z,name) = 
      test x {Expect.equal y z name}

[<Tests>]
let lambdaRuntimeTestGroup = testList "Parser Test Group" (List.map makeMyTests parserTestDescriptions)
[<Tests>]
let parserTestGroup = testList "LambdaRuntime Test Group" (List.map makeMyTests test_List_Lambda_Runtime)
[<Tests>]
let bindEmptyVariablesTestGroup = testList "bindEmptyVariables Test Group" (List.map makeMyTests bindEmptyVariablesDescriptions)
[<Tests>]
let matchTestGroup = testList "Match Test Group" (List.map makeMyTests testMatchDescriptions)
[<Tests>]
let bindPairHelperTestGroup = testList "bindPairHelper Test Group" (List.map makeMyTests bindPairHelperDescriptions)
[<Tests>]
let lambdaEvalTestGroup = testList "Top Functions Test Group" (List.map makeMyTests testLambdaEvalDescriptions)

[<EntryPoint>]  
let main argv =
    // THE FOLLOWING FILE IS TO RUN WITH THE LAMBDA RUNTIME
    //execFile lambdaEval (Environment.CurrentDirectory + "/demoLamb.TSHARP")
    // THE FOLLOWING FILE IS TO RUN WITH THE COMBINATOR RUNTIME
    // execFile combinatorEval ("/Users/elliott/F#/hlp2020-07/lex_parse/demoComb.TSHARP")
    
    runTestsInAssembly defaultConfig [||] |> ignore
    Console.ReadKey() |> ignore
    0