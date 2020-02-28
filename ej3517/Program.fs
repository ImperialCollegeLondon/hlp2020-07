// Learn more about F# at http://fsharp.org
open System
open Expecto
open CombinatorRuntimeModule

let print x = 
    printfn "%A" x

//1- let f x = x + 2 in f 3
let lambdaAddition = FuncDefExp{Name = ['f']; Body = Lambda{InputVar = ['x']; Body = FuncApp( FuncApp( BFunc (Math Add), Var['x']), Literal(Int 2))}; Expression =  FuncApp( Var ['f'], Literal(Int 3))}
//2- let f x = Explode x in f "aloha"
let testExplode = FuncDefExp{Name = ['f']; Body = Lambda{InputVar = ['x']; Body = FuncApp( BFunc Explode, Var['x'])}; Expression = FuncApp( Var ['f'], Literal( String "aloha"))}
//3- let f x = Implode x in f ['a','l','o','h','a']
let testImplode = FuncDefExp{Name = ['f']; Body = Lambda{InputVar = ['x']; Body = FuncApp( BFunc Implode, Var['x'])}; Expression = FuncApp( Var ['f'], Pair(Literal (String "a"),Pair(Literal (String "l"),Pair(Literal (String "o"),Pair (Literal (String "h"),Pair (Literal (String "a"),Null))))))}
//4- ("aloha" = "aloha") = True
let testEqualString = FuncApp( FuncApp( BFunc Equal, Literal( String "aloha")),Literal( String "aloha"))
//5- (1 = 1) -> True
let testEqualInt = FuncApp( FuncApp( BFunc Equal, Literal( Int 1)),Literal( Int 1))
//6- (Null = Null) = True
let testEqualNull = FuncApp( FuncApp( BFunc Equal, Null),Null)
//7- True 1 "aloha" = 1
let testTrue = FuncApp( FuncApp( BFunc True, Literal(Int 1 )),Literal( String "aloha"))
//8- False 1 "aloha" = "aloha"
let testFalse = FuncApp( FuncApp( BFunc False, Literal(Int 1 )),Literal( String "aloha"))
//9- "aloha" = "aloha" 1 "aloha" = 1
let testEqTrue = FuncApp( FuncApp( FuncApp( FuncApp( BFunc Equal, Literal( String "aloha")),Literal( String "aloha")), Literal(Int 1 )),Literal( String "aloha"))
//10- let f x = x - 1 in let g y = y * 3 in g ( f -1 ) = -6
let testNestedFunc =
    FuncDefExp{Name = ['f']; Body = Lambda{InputVar = ['x']; Body = FuncApp( FuncApp( BFunc (Math Sub), Var ['x']), Literal( Int 1))}; 
        Expression = FuncDefExp{ Name = ['g']; Body = Lambda{ InputVar = ['y']; Body = FuncApp( FuncApp( BFunc (Math Mult), Var ['y']), Literal( Int 3))};
            Expression = FuncApp( Var ['g'], FuncApp( Var ['f'], Literal(Int -1)))}}
//11- (let h p = p^2 in let g n = n * 2 in let f m = g m + g (m + 1) + h ( h m ) in (if True then f 2 else Null)) = 26
// h p
let lambdaH = Lambda{InputVar = ['p']; Body = FuncApp( FuncApp(BFunc (Math Mult), Var ['p']), Var ['p'])}
// g n
let lambdaG = Lambda{InputVar = ['n']; Body = FuncApp( FuncApp(BFunc (Math Mult), Var ['n']), Literal( Int 2))} //works 
// f m
let funcabc a b c = FuncApp( FuncApp( BFunc (Math Add), FuncApp( FuncApp( BFunc (Math Add), a), b)), c)
let gm = FuncApp( Var ['g'], Var ['m'])
let lit1 = Literal( Int 1)
let gm1 = FuncApp( Var ['g'], FuncApp( FuncApp( BFunc (Math Add), Var ['m']), lit1))
let hhm = FuncApp(Var ['h'], FuncApp( Var['h'], Var['m']))
let lambdaF = Lambda{InputVar = ['m']; Body = funcabc gm gm1 hhm}
// f 2
let fapp = FuncApp( Var ['f'], Literal( Int 2))
// IfThenElse
let ifthen = FuncApp( FuncApp( FuncApp( BFunc IfThenElse, (BFunc True)), fapp), Null)
//
let expreF = FuncDefExp{Name = ['f']; Body = lambdaF; Expression = ifthen}
let expreG = FuncDefExp{ Name = ['g']; Body = lambdaG; Expression = expreF}
let finalExpression = FuncDefExp{ Name = ['h']; Body = lambdaH; Expression = expreG}
// Test pipeline 
// let expreH = finalExpression |> Some
// let expreHres = Option.map Reduce expreH


let (testDescriptions : list<string * Result<AST,string> * Result<AST,string> * string>) = [
    ("simple addition let f x = x+2 in f 3", Reduce lambdaAddition, Ok (Literal (Int 5)), "f 3 =  Ok (Literal (Int 5))");
    ("Explode let f x = Explode x in f \"aloha\"", Reduce testExplode, Ok(Pair(Literal (String "a"),Pair(Literal (String "l"),Pair(Literal (String "o"),Pair (Literal (String "h"),Pair (Literal (String "a"),Null)))))), "f \"aloha\" =  Ok (['a','l','o','h','a'])");
    ("Explode let f x = Implode x in f ['a','l','o','h','a']", Reduce testImplode, Ok (Literal (String "aloha")), "f ['a','l','o','h','a'] =  Ok (Literal (String \"aloha\"))");
    ("Equal : (\"aloha\" = \"aloha\") = True", Reduce testEqualString, Ok (BFunc True), "(\"aloha\" = \"aloha\") = True");
    ("Equal : (1 = 1) = True", Reduce testEqualInt, Ok (BFunc True), "(1 = 1) = True");
    ("Equal : (Null = Null) = True", Reduce testEqualNull, Ok (BFunc True), "(Null = Null) = True");
    ("True : True 1 \"aloha\" = 1", Reduce testTrue, Ok (Literal(Int 1)), "False 1 \"aloha\" = 1");
    ("False 1 \"aloha\" = \"aloha\"", Reduce testFalse, Ok (Literal( String "aloha")), "False 1 \"aloha\" = \"aloha\"");
    ("(\"aloh\" = \"aloha\") 1 \"aloha\" = 1", Reduce testEqTrue, Ok (Literal( Int 1)), "(\"aloh\" = \"aloha\") 1 \"aloha\" = \"aloha\"");
    ("Nested Function : (let f x = x - 1 in let g y = y * 3 in g ( f -1 )) = -6", Reduce testNestedFunc, Ok (Literal( Int -6)), "(let f x = x - 1 in let g y = y * 3 in g ( f -1 )) = -6");
    ("(let h p = p^2 in let g n = n * 2 in let f m = g m + g (m + 1) + h ( h m ) in (if True then f 2 else Null)) = 26", Reduce finalExpression, Ok(Literal(Int 26)), "(let h p = p^2 in let g n = n * 2 in let f m = g m + g (m + 1) + h ( h m ) in (if True then f 2 else Null)) = 26")]

let makeMyTests (x,y,z,name) = 
    test x {Expect.equal y z name}

[<Tests>]
let makeem = testList "A test group" (List.map makeMyTests testDescriptions)

let allTestsWithExpecto() =
        runTestsInAssembly defaultConfig [||]

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    allTestsWithExpecto() |> ignore
    0 // return an integer exit code
