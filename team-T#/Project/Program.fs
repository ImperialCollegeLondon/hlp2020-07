open TopModule
open TestsModule
open System
open System.IO
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
    printf "Enter the path to a .TSHARP file to execute: "
    let path = Console.ReadLine()
    execFile lambdaEval path
    runTestsInAssembly defaultConfig [||] |> ignore
    Console.ReadKey() |> ignore
    0