// Learn more about F# at http://fsharp.org
open Lambdas
open System
open Expecto

[<Tests>]
let testListWithExpecto =
    testList "Test Group with Expecto" [
        test "Runtime Test 0" {
          Expect.equal (exec test0) (Error "Run time error: Funcapp (Literal (Int 3),Literal (Int 2)) is not a valid function application") "3(2(1))"
        }
        test "Runtime Test 1" {
            Expect.equal (exec test1) (Ok(Literal (Int 10))) "(fun x -> x+1) 9"        
        }
        test "Runtime Test 2" {
            Expect.equal (exec test2) (Error "Run-time error: Cannot divide by 0!") "(120%0)/2+3"        
        }
        test "Runtime Test 3" {
            Expect.equal (exec test3) (Ok(Literal (Int 12))) "((fun a -> fun b -> a + b/a)3)27"        
        }
        test "Runtime Test 4" {
            Expect.equal (exec test4) (Error "Run-time error: ['n'] is not defined") "f a b = a+n/b in f 4 36"        
        }
        test "Runtime Test 5" {
            Expect.equal (exec test5) (Ok(Literal (Int 125))) "let f = 5 in f*f*f"        
        }
        test "Runtime Test 6" {
            Expect.equal (exec test6) (Error "Run-time error: Literal (Int 9) was expexted to be a pair") "let f a b = a + b/a in f 3 (Implode 9 9)"        
        }
        test "Runtime Test 7" {
            Expect.equal (exec test7) (Ok(Lambda { InputVar = ['b']; Body = Funcapp(Funcapp(BuiltInFunc (Mat Add),Funcapp(Funcapp (BuiltInFunc (Mat Mult),Literal (Int 3)),Literal (Int 3))),Var ['b'])})) "let f a b = a*a + b in let g x = f x in g 3 "      
        }
        test "Runtime Test 8" {
            Expect.equal (exec test8) (Ok(Literal (Int 6))) "let f x = x+1 in let g y = y+2 in g (f 3)"        
        }
        test "Runtime Test 9" {
            Expect.equal (exec test9) (Error "Run-time error: Add(Literal (String ['a']),Literal (Int 1)) , is not a valid expression") """let f x = x+1 in f (f "a")"""        
        }
        test "Runtime Test 10" {
            Expect.equal (exec test10) (Error "Run-time error: Add(Literal (String ['a']),Literal (Int 1)) , is not a valid expression") "if 2=1 then test9 else test8"        
        }
        test "Runtime Test 11" {
            Expect.equal (exec test11) (Ok(Literal (Int 6))) "if 2=2 then test9 else test8"        
        }
        test "Runtime Test 12" {
            Expect.equal (exec test12) (Ok(Literal (Int 120))) "FACTORIAL: let rec f n = if n = 0 then 1 else n*f(n-1) in f 5"        
        }
        test "Runtime Test 13" {
            Expect.equal (exec test13) (Ok(Literal (Int 75025))) "FIBONACCI: let rec f a = if a = 0 then 0 else if a = 1 then 1 else f(a-1) + f(a-2) in f 25"        
        }

    ]

let allTestsWithExpecto() =
    runTestsInAssembly defaultConfig [||]

[<EntryPoint>]
let main argv =
    printfn "Testing with FSCheck and Expecto!"
    allTestsWithExpecto() |> ignore
    (*printfn "%A" result0
    printfn "%A" result1
    printfn "%A" result2
    printfn "%A" result3
    printfn "%A" result4
    printfn "%A" result5
    printfn "%A" result6
    printfn "%A" result7
    printfn "%A" result8
    printfn "%A" result9
    printfn "%A" result10
    printfn "%A" result11
    printfn "%A" result12
    printfn "%A" result13*)

    Console.ReadKey() |> ignore // not needed running from Visual Studio
    0 // return an integer exit code



