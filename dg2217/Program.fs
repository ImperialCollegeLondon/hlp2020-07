open Expecto
open System
open Lambdas

let test0 = FuncApp (FuncApp (Literal(Int 3L),Literal (Int 2L)),Literal (Int 1L))

let test1 = FuncApp(Lambda{InputVar = ['x']; Body = FuncApp(FuncApp(BFunc(Mat Add),Var(['x'])),Literal(Int(1L)))},Literal(Int(9L)))

let simpleMod = (FuncApp(FuncApp(BFunc(Mat Mod),Literal(Int(120L))),Literal(Int(0L))))
let simpleDiv = (FuncApp(FuncApp(BFunc(Mat Div),simpleMod),Literal(Int(2L))))
let test2 = FuncApp(FuncApp(BFunc(Mat Add),simpleDiv),Literal(Int(3L)))

let div3 = (FuncApp(FuncApp(BFunc(Mat Div),Var(['b'])),Var(['a'])))
let add3 = (FuncApp(FuncApp(BFunc(Mat Add),Var(['a'])),div3))
let ABbody = Lambda {InputVar = ['a']; Body = Lambda{InputVar = ['b'];Body = add3}}
let test3 = FuncApp(FuncApp(ABbody,Literal(Int(3L))),Literal(Int(27L)))

let div4 = (FuncApp(FuncApp(BFunc(Mat Div),Var(['n'])),Var(['a'])))
let add4 = (FuncApp(FuncApp(BFunc(Mat Add),Var(['a'])),div4))
let ABbody4 = Lambda {InputVar = ['a']; Body = Lambda{InputVar = ['b'];Body = add4}}
let FBody = FuncApp(FuncApp(Var(['f']),Literal(Int(4L))),Literal(Int(36L)))
let test4 = FuncDefExp{Name = ['f'];Body = ABbody4;Expression = FBody}
// let f a b = a + b/a in f 3 27

let test5FBody = FuncApp(FuncApp(BFunc (Mat Mult), FuncApp(FuncApp(BFunc (Mat Mult), Var ['f']),Var['f'])),Var ['f'])
let test5 = FuncDefExp{Name = ['f'];Body = Literal(Int(5L)); Expression = test5FBody}
// let f = 5 in f*f*f

let test6FBody = FuncApp(FuncApp(Var(['f']),Literal(Int(3L))),FuncApp(FuncApp(BFunc(Implode),Literal(Int(9L))),Literal(Int(9L))))
let test6 = FuncDefExp{Name = ['f']; Body = ABbody; Expression =  test6FBody} 
// let f a b = a + b/a in f 3 (9*9)

let test7GBody = Lambda{InputVar = ['x'];Body = FuncApp(Var['f'],Var ['x'])}
let test7FBody =Lambda{InputVar = ['a'];Body = Lambda{InputVar = ['b']; Body = FuncApp(FuncApp(BFunc (Mat Add),FuncApp(FuncApp(BFunc (Mat Mult), Var ['a']),Var['a'])),Var['b'])}} 
let test7 = FuncDefExp{Name = ['f'];Body = test7FBody;Expression = FuncDefExp{Name = ['g'];Body = test7GBody;Expression = (FuncApp(Var ['g'],Literal(Int 3L)))}}  
// let f a b = a*a + b in let g x = f x in g 3 

let test8 = FuncDefExp{Name = ['f']; Body = Lambda{InputVar = ['x'];Body = FuncApp(FuncApp(BFunc (Mat Add), Var ['x']),Literal (Int 1L))};Expression = FuncDefExp{Name = ['g'];Body = Lambda{InputVar = ['y']; Body = FuncApp (FuncApp (BFunc (Mat Add), Var ['y']), Literal (Int 2L))};Expression = (FuncApp(Var ['f'], FuncApp(Var['g'], Literal(Int 3L))))}} 
//let f x = x+1 in let g y = y+2 in g (f 3)

let test9 = FuncDefExp{Name = ['f'];Body = Lambda{InputVar = ['x']; Body = FuncApp(FuncApp(BFunc (Mat Add), Var ['x']),Literal (Int 1L))};Expression = FuncApp (Var['f'],(FuncApp (Var ['f'], Literal(String ['a']))))}  
// let f x = x+1 in f (f "a")

let test10 = FuncApp(FuncApp(FuncApp(FuncApp(BFunc(Equal),Literal(Int 2L)),Literal(Int 1L)),Lazy(test8)),Lazy(test9))
// if 2=1 then test9 else test8
let test11 = FuncApp(FuncApp(FuncApp(FuncApp(BFunc(Equal),Literal(Int 2L)),Literal(Int 2L)),Lazy(test8)),Lazy(test9))
// if 2=2 then test9 else test8

let elseBody12 = FuncApp(FuncApp(BFunc(Mat Mult),Var ['n']),FuncApp(Var ['f'], FuncApp(FuncApp(BFunc(Mat Sub),Var ['n']),Literal(Int 1L))))
let ifStatement12 = FuncApp(FuncApp(FuncApp(FuncApp(BFunc Equal,Var ['n']),Literal(Int 0L)),Lazy(Literal(Int 1L))),Lazy(elseBody12))
let fBody12 = Lambda{InputVar = ['f']; Body = Lambda{InputVar = ['n']; Body = ifStatement12}}
let test12 = FuncDefExp{Name = ['f'];Body = fBody12;Expression = FuncApp(FuncApp(Var ['f'],Lazy(FuncApp(Y,Var['f']))),Literal(Int 5L))}
// let rec f n = if n = 0 then 1 else n*f(n-1) in f 2 SAME AS let f' f n = if n = 0 then 1 else n*f(n-1) in f' (Y h) 2 

let fminus1 = FuncApp(Var ['f'], FuncApp(FuncApp(BFunc (Mat Sub), Var ['a']),Literal (Int 1L)))
let fminus2 = FuncApp(Var ['f'], FuncApp(FuncApp(BFunc (Mat Sub), Var ['a']),Literal (Int 2L)))
let recBody = FuncApp(FuncApp(BFunc (Mat Add), fminus1),fminus2)
let eqBody1 = FuncApp(FuncApp(BFunc Equal, Var ['a']), Literal(Int 1L))
let eqBody0 = FuncApp(FuncApp(BFunc Equal, Var ['a']), Literal(Int 0L))
let ifelseBody13 = FuncApp(FuncApp(eqBody0,Lazy(Literal(Int 0L))),Lazy(FuncApp(FuncApp(eqBody1,Lazy(Literal(Int 1L))),Lazy(recBody)))) 
let test13 = 
    FuncDefExp{
        Name = ['f'];
        Body = Lambda{InputVar = ['f']; Body = Lambda {InputVar = ['a']; Body = ifelseBody13}};
        Expression = FuncApp(FuncApp(Var ['f'],Lazy(FuncApp(Y,Var['f']))),Literal(Int 25L))} 
//fibonacci
//let rec f a = if a = 0 then 0 else if a = 1 then 1 else f(a-1) + f(a-2) in f 25
let tailBody14 = FuncApp(Var ['f'], FuncApp(BFunc (PSnd), Var ['p']))
let headBody14 = FuncApp(FuncApp(BFunc (Mat Mult),FuncApp(BFunc PFst,(Var['p']))),Literal (Int 2L))
let elseBody14 = FuncApp(FuncApp(BFunc P, Lazy(headBody14)),Lazy(tailBody14))
let eqBody14 = FuncApp(FuncApp(BFunc Equal, Var ['p']), Null)
let ifelseBody14 = FuncApp(FuncApp(eqBody14,Lazy(Null)),Lazy(elseBody14)) 
let list14 = Pair(Literal(Int 1L), Pair(FuncApp(FuncApp(BFunc(Mat Mult), Literal(Int 3L)),Literal(Int 2L)), Pair(Literal(Int 2L), Pair(test12, Null))))
let test14 =
    FuncDefExp{
        Name = ['f'];
        Body = Lambda{InputVar = ['f']; Body = Lambda {InputVar = ['p']; Body = ifelseBody14}};
        Expression = FuncApp(FuncApp(Var ['f'],Lazy(FuncApp(Y,Var['f']))),list14)}  
//let rec f p = if p = [] then [] else  ((p.Head)*2)::f(p.Tail) in f [1;2*3;3;test12]

[<Tests>]
let testListWithExpecto =
    testList "Test Group with Expecto" [
        test "Runtime Test 0" {
          Expect.equal (exec test0) (Error "Run time error: FuncApp (Literal (Int 3L),Literal (Int 2L)) is not a valid function application") "3(2(1))"
        }
        test "Runtime Test 1" {
            Expect.equal (exec test1) (Ok(Literal (Int 10L))) "(fun x -> x+1) 9"        
        }
        test "Runtime Test 2" {
            Expect.equal (exec test2) (Error "Run-time error: Cannot divide by 0!") "(120%0)/2+3"        
        }
        test "Runtime Test 3" {
            Expect.equal (exec test3) (Ok(Literal (Int 12L))) "((fun a -> fun b -> a + b/a)3)27"        
        }
        test "Runtime Test 4" {
            Expect.equal (exec test4) (Error "Run-time error: ['n'] is not defined") "f a b = a+n/b in f 4 36"        
        }
        test "Runtime Test 5" {
            Expect.equal (exec test5) (Ok(Literal (Int 125L))) "let f = 5 in f*f*f"        
        }
        test "Runtime Test 6" {
            Expect.equal (exec test6) (Error "Run-time error: Literal (Int 9L) is not a valid list to implode") "let f a b = a + b/a in f 3 (Implode 9 9)"        
        }
        test "Runtime Test 7" {
            Expect.equal (exec test7) (Ok(Lambda { InputVar = ['b']; Body = FuncApp(FuncApp(BFunc (Mat Add),FuncApp(FuncApp (BFunc (Mat Mult),Literal (Int 3L)),Literal (Int 3L))),Var ['b'])})) "let f a b = a*a + b in let g x = f x in g 3 "      
        }
        test "Runtime Test 8" {
            Expect.equal (exec test8) (Ok(Literal (Int 6L))) "let f x = x+1 in let g y = y+2 in g (f 3)"        
        }
        test "Runtime Test 9" {
            Expect.equal (exec test9) (Error "Run-time error: Add(Literal (String ['a']),Literal (Int 1L)) , is not a valid expression") """let f x = x+1 in f (f "a")"""        
        }
        test "Runtime Test 10" {
            Expect.equal (exec test10) (Error "Run-time error: Add(Literal (String ['a']),Literal (Int 1L)) , is not a valid expression") "if 2=1 then test9 else test8"        
        }
        test "Runtime Test 11" {
            Expect.equal (exec test11) (Ok(Literal (Int 6L))) "if 2=2 then test9 else test8"        
        }
        test "Runtime Test 12" {
            Expect.equal (exec test12) (Ok(Literal (Int 120L))) "FACTORIAL: let rec f n = if n = 0 then 1 else n*f(n-1) in f 5"        
        }
        test "Runtime Test 13, FIBONACI" {
            Expect.equal (exec test13) (Ok(Literal (Int 75025L))) "FIBONACCI: let rec f a = if a = 0 then 0 else if a = 1 then 1 else f(a-1) + f(a-2) in f 25"        
        }
        test "Runtime Test 14, LIST RECURSIVE OPERATION" {
            Expect.equal (exec test14) (Ok(Pair(Literal (Int 2L),Pair(Literal (Int 12L),Pair (Literal (Int 4L),Pair (Literal (Int 240L),Null)))))) "LISTS & RECURSION: let rec f p = if p = [] then [] else  ((p.Head)*2)::f(p.Tail) in f [1;2*3;3;let rec f n = if n = 0 then 1 else n*f(n-1) in f 5]"        
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



