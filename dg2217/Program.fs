// Learn more about F# at http://fsharp.org
open System
open Lambdas
open Expecto

let test0 = Funcapp (Funcapp (Literal(Int 3),Literal (Int 2)),Literal (Int 1))

let test1 = Funcapp(Lambda{InputVar = ['x']; Body = Funcapp(Funcapp(BuiltInFunc(Mat Add),Var(['x'])),Literal(Int(1)))},Literal(Int(9)))

let simpleMod = (Funcapp(Funcapp(BuiltInFunc(Mat Mod),Literal(Int(120))),Literal(Int(0))))
let simpleDiv = (Funcapp(Funcapp(BuiltInFunc(Mat Div),simpleMod),Literal(Int(2))))
let test2 = Funcapp(Funcapp(BuiltInFunc(Mat Add),simpleDiv),Literal(Int(3)))

let div3 = (Funcapp(Funcapp(BuiltInFunc(Mat Div),Var(['b'])),Var(['a'])))
let add3 = (Funcapp(Funcapp(BuiltInFunc(Mat Add),Var(['a'])),div3))
let ABbody = Lambda {InputVar = ['a']; Body = Lambda{InputVar = ['b'];Body = add3}}
let test3 = Funcapp(Funcapp(ABbody,Literal(Int(3))),Literal(Int(27)))

let div4 = (Funcapp(Funcapp(BuiltInFunc(Mat Div),Var(['n'])),Var(['a'])))
let add4 = (Funcapp(Funcapp(BuiltInFunc(Mat Add),Var(['a'])),div4))
let ABbody4 = Lambda {InputVar = ['a']; Body = Lambda{InputVar = ['b'];Body = add4}}
let FBody = Funcapp(Funcapp(Var(['f']),Literal(Int(4))),Literal(Int(36)))
let test4 = FuncDefExp{Name = ['f'];Body = ABbody4;Expression = FBody}
// let f a b = a + b/a in f 3 27

let test5FBody = Funcapp(Funcapp(BuiltInFunc (Mat Mult), Funcapp(Funcapp(BuiltInFunc (Mat Mult), Var ['f']),Var['f'])),Var ['f'])
let test5 = FuncDefExp{Name = ['f'];Body = Literal(Int(5)); Expression = test5FBody}
// let f = 5 in f*f*f

let test6FBody = Funcapp(Funcapp(Var(['f']),Literal(Int(3))),Funcapp(Funcapp(BuiltInFunc(Implode),Literal(Int(9))),Literal(Int(9))))
let test6 = FuncDefExp{Name = ['f']; Body = ABbody; Expression =  test6FBody} 
// let f a b = a + b/a in f 3 (9*9)

let test7GBody = Lambda{InputVar = ['x'];Body = Funcapp(Var['f'],Var ['x'])}
let test7FBody =Lambda{InputVar = ['a'];Body = Lambda{InputVar = ['b']; Body = Funcapp(Funcapp(BuiltInFunc (Mat Add),Funcapp(Funcapp(BuiltInFunc (Mat Mult), Var ['a']),Var['a'])),Var['b'])}} 
let test7 = FuncDefExp{Name = ['f'];Body = test7FBody;Expression = FuncDefExp{Name = ['g'];Body = test7GBody;Expression = (Funcapp(Var ['g'],Literal(Int 3)))}}  
// let f a b = a*a + b in let g x = f x in g 3 

let test8 = FuncDefExp{Name = ['f']; Body = Lambda{InputVar = ['x'];Body = Funcapp(Funcapp(BuiltInFunc (Mat Add), Var ['x']),Literal (Int 1))};Expression = FuncDefExp{Name = ['g'];Body = Lambda{InputVar = ['y']; Body = Funcapp (Funcapp (BuiltInFunc (Mat Add), Var ['y']), Literal (Int 2))};Expression = (Funcapp(Var ['f'], Funcapp(Var['g'], Literal(Int 3))))}} 
//let f x = x+1 in let g y = y+2 in g (f 3)

let test9 = FuncDefExp{Name = ['f'];Body = Lambda{InputVar = ['x']; Body = Funcapp(Funcapp(BuiltInFunc (Mat Add), Var ['x']),Literal (Int 1))};Expression = Funcapp (Var['f'],(Funcapp (Var ['f'], Literal(String ['a']))))}  
// let f x = x+1 in f (f "a")

let test10 = Funcapp(Funcapp(Funcapp(Funcapp(BuiltInFunc(Equal),Literal(Int 2)),Literal(Int 1)),Lazy(test8)),Lazy(test9))
// if 2=1 then test9 else test8
let test11 = Funcapp(Funcapp(Funcapp(Funcapp(BuiltInFunc(Equal),Literal(Int 2)),Literal(Int 2)),Lazy(test8)),Lazy(test9))
// if 2=2 then test9 else test8

let elseBody12 = Funcapp(Funcapp(BuiltInFunc(Mat Mult),Var ['n']),Funcapp(Var ['f'], Funcapp(Funcapp(BuiltInFunc(Mat Sub),Var ['n']),Literal(Int 1))))
let ifStatement12 = Funcapp(Funcapp(Funcapp(Funcapp(BuiltInFunc Equal,Var ['n']),Literal(Int 0)),Lazy(Literal(Int 1))),Lazy(elseBody12))
let fBody12 = Lambda{InputVar = ['f']; Body = Lambda{InputVar = ['n']; Body = ifStatement12}}
let test12 = FuncDefExp{Name = ['f'];Body = fBody12;Expression = Funcapp(Funcapp(Var ['f'],Lazy(Funcapp(Y,Var['f']))),Literal(Int 5))}
// let rec f n = if n = 0 then 1 else n*f(n-1) in f 2 SAME AS let f' f n = if n = 0 then 1 else n*f(n-1) in f' (Y h) 2 

let fminus1 = Funcapp(Var ['f'], Funcapp(Funcapp(BuiltInFunc (Mat Sub), Var ['a']),Literal (Int 1)))
let fminus2 = Funcapp(Var ['f'], Funcapp(Funcapp(BuiltInFunc (Mat Sub), Var ['a']),Literal (Int 2)))
let recBody = Funcapp(Funcapp(BuiltInFunc (Mat Add), fminus1),fminus2)
let eqBody1 = Funcapp(Funcapp(BuiltInFunc Equal, Var ['a']), Literal(Int 1))
let eqBody0 = Funcapp(Funcapp(BuiltInFunc Equal, Var ['a']), Literal(Int 0))
let ifelseBody13 = Funcapp(Funcapp(eqBody0,Lazy(Literal(Int 0))),Lazy(Funcapp(Funcapp(eqBody1,Lazy(Literal(Int 1))),Lazy(recBody)))) 
let test13 = 
    FuncDefExp{
        Name = ['f'];
        Body = Lambda{InputVar = ['f']; Body = Lambda {InputVar = ['a']; Body = ifelseBody13}};
        Expression = Funcapp(Funcapp(Var ['f'],Lazy(Funcapp(Y,Var['f']))),Literal(Int 25))} 
//fibonacci
//let rec f a = if a = 0 then 0 else if a = 1 then 1 else f(a-1) + f(a-2) in f 25
let tailBody14 = Funcapp(Var ['f'], Funcapp(BuiltInFunc (PSnd), Var ['p']))
let headBody14 = Funcapp(Funcapp(BuiltInFunc (Mat Mult),Funcapp(BuiltInFunc PFst,(Var['p']))),Literal (Int 2))
let elseBody14 = Funcapp(Funcapp(BuiltInFunc P, Lazy(headBody14)),Lazy(tailBody14))
let eqBody14 = Funcapp(Funcapp(BuiltInFunc Equal, Var ['p']), Null)
let ifelseBody14 = Funcapp(Funcapp(eqBody14,Lazy(Null)),Lazy(elseBody14)) 
let list14 = Pair(Literal(Int 1), Pair(Funcapp(Funcapp(BuiltInFunc(Mat Mult), Literal(Int 3)),Literal(Int 2)), Pair(Literal(Int 2), Pair(test12, Null))))
let test14 =
    FuncDefExp{
        Name = ['f'];
        Body = Lambda{InputVar = ['f']; Body = Lambda {InputVar = ['p']; Body = ifelseBody14}};
        Expression = Funcapp(Funcapp(Var ['f'],Lazy(Funcapp(Y,Var['f']))),list14)}  
//let rec f p = if p = [] then [] else  ((p.Head)*2)::f(p.Tail) in f [1;2*3;3;test12]

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
            Expect.equal (exec test6) (Error "Run-time error: Literal (Int 9) is not a valid list to implode") "let f a b = a + b/a in f 3 (Implode 9 9)"        
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
        test "Runtime Test 13, FIBONACI" {
            Expect.equal (exec test13) (Ok(Literal (Int 75025))) "FIBONACCI: let rec f a = if a = 0 then 0 else if a = 1 then 1 else f(a-1) + f(a-2) in f 25"        
        }
        test "Runtime Test 14, LIST RECURSIVE OPERATION" {
            Expect.equal (exec test14) (Ok(Pair(Literal (Int 2),Pair(Literal (Int 12),Pair (Literal (Int 4),Pair (Literal (Int 240),Null)))))) "LISTS & RECURSION: let rec f p = if p = [] then [] else  ((p.Head)*2)::f(p.Tail) in f [1;2*3;3;let rec f n = if n = 0 then 1 else n*f(n-1) in f 5]"        
        }


    ]

let allTestsWithExpecto() =
    runTestsInAssembly defaultConfig [||]

[<EntryPoint>]
let main argv =
    printfn "Testing with Expecto!"
    allTestsWithExpecto() |> ignore
    Console.ReadKey() |> ignore // not needed running from Visual Studio
    0 // return an integer exit code



