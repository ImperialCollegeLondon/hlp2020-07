module ParserModule

let test() = printfn "sample top level function in ParserModule" 
type AdditiveOperator = Sum of char list | Subtraction of char list 
type MultiplicativeOperator = Multiplication of char list | Division of char list 
// <item> ::= <numeric-literal> | <string-literal> | <bracketed-additive-exp> | <if-exp>

// <applicative-exp> ::= <item> | <item> <applicative-exp>

// <multiplicative-exp> ::= <applicative-exp> | <applicative-exp> <multiplicative-op> <multiplicative-exp>

// <additive-exp> ::= <multiplicative-exp> | <multiplicative-exp> <additive-op> <additive-exp>

// <multiplicative-op> ::= "*" | "/"

// <additive-op> ::= "+ | "-"

//ordered from high to low precedence 
type Precedence = 
    | AdditiveExpression of Precedence //level 4
    | MultiplicativeExpression of Precedence //level 3
    | ApplicativeExpression of Precedence  //level 2
    | BracketedAdditiveExpression of Precedence //level 1 behaves like additive exp
    | IfExpression of Precedence // level 1 
    | Other of char list // level 1 int string params etc
    // how to implement function definition in here?

type BuiltInType = 
    | Add 
    | Sub 
    | Mult 
    | Div
    | Mod
    | Test 
    | Equal //works for strings ints and nulls 
    | Explode 
    | Implode 
    | P //creates a pair 
    | PFst 
    | PSnd
    | IsPair
    | IfThenElse  
    
type AST = 
    | FuncDefExp of FuncDefExpType 
    | Lambda of LambdaType: (char list)*Body:AST
    | Var of char list //only valid in lambdas 
    | Funcapp of AST*AST
    | Pair of AST*AST 
    | Null 
    | Literal of LitType 
    | BuiltInFunc of BuiltInType
    | Param of char list

and LitType = 
    | Int of int 
    | String of string
    | True of AST //make it a named function with Lambda ("A",Lambda ("B",Var "A" ))
    | False of AST // Lambda ("A",Lambda ("B",Var "A" ))
//    | Combinator of CombinatorType do later 

//FuncApp(FuncApp(Plus, exp1), exp2) ==> exp1 + exp2

//fun f -> fun x -> fun y -> f y x ==>
//Lambda("f", Lambda( "x", Lambda("y",FuncApp(FuncApp (Var "f", Var "y"), Var "x"))))

