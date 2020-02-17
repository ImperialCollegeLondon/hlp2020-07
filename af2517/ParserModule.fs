module ParserModule

let test() = printfn "sample top level function in ParserModule" 

type BuiltInType = 
    | Add 
    | Sub 
    | Mult 
    | Div
    | Test 
    | IntEQ
    | Bigger
    | BiggerOrEqual
    | Smaller 
    | SmallerOrEqual
    | StringEQ
    | StringToCharList
    | CharListToString

type LitType = 
    | Int of int 
    | String of string 

type AST = 
    | FuncDefExp of FuncDefExpType 
    | Lambda of LambdaType: (char list)*Body:AST
    | Var of char list //only valid in lambdas 
    | Funcapp of AST*AST
    | Pair of AST*AST 
    | Null 
    | Literal of LitType 
    | BuiltInFunc of BuiltInType
    | Param of char list //when a parameter is given for funcapp whose type is unknown bc
    // it was previously defined 
//    | Combinator of CombinatorType do later 

//FuncApp(FuncApp(Plus, exp1), exp2) ==> exp1 + exp2

//fun f -> fun x -> fun y -> f y x ==>
//Lambda("f", Lambda( "x", Lambda("y",FuncApp(FuncApp (Var "f", Var "y"), Var "x"))))

