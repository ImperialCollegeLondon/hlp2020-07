module Lambdas

type ReductionType = |Normal|Applicative

type MathType = |Add|Sub|Mult|Div|Mod

type BuiltInType = 
    | Math of MathType
    | Equal //works for strings ints and nulls 
    | Explode 
    | Implode 
    | P //creates a pair 
    | PFst 
    | PSnd
    | IsPair
    | IfThenElse  


type AST = 
    | FuncDefExp of FuncDefExpType:char list*AST*AST 
    | Lambda of LambdaType
    | Var of char list //only valid in lambdas 
    | Funcapp of AST*AST
    | Pair of AST*AST 
    | Null 
    | Literal of LitType 
    | BuiltInFunc of BuiltInType

and LambdaType = {
    InputVar: char list
    Body: AST
}
and LitType = 
    | Int of int 
    | String of char list 
    | True of AST //make it a named function with Lambda("A",Lambda ("B",Var "A" ))
    | False of AST 

and EnvironmentType = list<(char list)*AST>

let trueAST = Lambda {InputVar = ['x']; Body = Lambda {InputVar = ['y'];Body = Var ['x']}}
let falseAST = Lambda {InputVar = ['x']; Body = Lambda {InputVar = ['y']; Body = Var ['y']}}

let concat s1 s2 =
    match (s1,s2) with
    | (Literal(String(a)),Literal(String(b))) -> Literal(String(a@b))

let rec execImplode lst = 
    match lst with
    | Null -> Literal(String([]))
    | Pair(Literal(String([c])),p) -> (Literal(String [c]), (execImplode p)) ||> concat
    | _ -> Null //Error

let rec execExplode (str:AST) =
    match str with
    | Literal (String []) -> Null
    | Literal (String (hd::tl)) -> Pair(Literal(String([hd])), Literal(String tl) |> execExplode)
    | _ -> Null // Error 

concat (Literal(String['a';'b'])) (Literal(String ['c';'d']))
execExplode (Literal(String ['a';'b';'c';'d']))
execImplode (Pair(Literal (String ['a']),Pair(Literal (String ['b']),Pair (Literal (String ['c']),Pair (Literal (String ['d']),Null)))))

let rec findValue (env:EnvironmentType) name=
    match env with
    | (n,v)::_ when n=name -> v
    | _::tl -> findValue tl name
    | _ -> printfn "%A is not defined" (Var name) ; Null//Var(name) 

let execEqual x y = 
    match (x,y) with
    | (Literal(Int _),Literal(Int _)) 
    | (Literal(String _),Literal(String _))
    | Null,Null -> 
        if x = y then trueAST else falseAST
    | _ -> Null //Error

let execMath op x y =
    match (x,y) with 
    |Int A,Int B ->
        match op with
        | Add -> A+B |> Int |> Literal //Literal(Int(valueA+valueB))
        | Sub -> A-B |> Int |> Literal
        | Mult -> A*B |> Int |> Literal
        | Div -> A/B |> Int |> Literal
        | Mod -> A%B |> Int |> Literal
    | _ -> Null//Funcapp(Funcapp(BuiltInFunc(Math op),x),y) //Error

let execPFst x = 
    match x with
    | Pair(a,_) -> a
    | _ -> Null //Error

let execPSnd x = 
    match x with
    | Pair(_,b) -> b
    | _ -> Null //Error

let execIsPair x =
    match x with | Pair(_) -> trueAST | _ -> falseAST

let (|MATCH2ARG|_|) exp = 
    match exp with
    | (Funcapp(Funcapp(BuiltInFunc(func),x),y)) -> Some (func, x, y)
    | _ -> None

let (|MATCH1ARG|_|) exp =
    match exp with
    | (Funcapp(BuiltInFunc(func),x)) -> Some (func, x)
    | _ -> None 

let rec exec ord (exp:AST) :AST =
    let rec betaReduce (env:EnvironmentType) exp =  
        let (|BASICS|_|) exp = 
            match exp with
            | Funcapp(Funcapp(churchBool,a),_) when churchBool = trueAST -> betaReduce env a |> Some
            | Funcapp(Funcapp(churchBool,_),b) when churchBool = falseAST -> betaReduce env b |> Some
            | Funcapp(Lambda l,E) -> 
                let newEnv = (l.InputVar, betaReduce env E)::env
                betaReduce newEnv l.Body |> Some
            | _ -> None      

        match exp with
        | FuncDefExp(name,body,E) -> betaReduce env (Funcapp(Lambda{InputVar = name; Body = E},body))
        | Var name  -> findValue env name 
        | Funcapp(ast1,ast2) ->
            match Funcapp(betaReduce env ast1, betaReduce env ast2) with
            | BASICS result -> result
            | x -> x
        | Lambda l -> Lambda {InputVar = l.InputVar; Body = betaReduce ((l.InputVar,Var l.InputVar)::env) l.Body}
        | x -> x 

    let rec evaluate exp =
        let basicFunctions exp = 
            match exp with 
            | MATCH2ARG (Equal,arg1,arg2) -> execEqual arg1 arg2 
            | MATCH2ARG (P,arg1,arg2)-> Pair(arg1,arg2) 
            | MATCH2ARG (Math op, Literal x, Literal y) -> execMath op x y 
            | MATCH1ARG (PFst,arg) -> execPFst arg 
            | MATCH1ARG (PSnd,arg) -> execPSnd arg 
            | MATCH1ARG (IsPair,arg) -> execIsPair arg 
            | MATCH1ARG (Implode,arg) ->  execImplode arg  
            | MATCH1ARG (Explode,arg) -> execExplode arg                
            | Funcapp(Literal(_), _) ->Null  // printfn "Cannot apply function to literal"
            | _ -> exp  

        match exp with
        | Funcapp(a,b) -> Funcapp(evaluate a,evaluate b) |> basicFunctions
        | Lambda l -> Lambda {InputVar = l.InputVar; Body = evaluate l.Body}        
        | x->x

    exp |> betaReduce [] |> evaluate

//let f a b = a+b/a in f 3.0 4.0
let test0 = Funcapp (Funcapp (BuiltInFunc (Math Add),Literal (Int 2)),Literal (Int 1))
let test1 = Funcapp(Lambda{InputVar = ['x']; Body = Funcapp(Funcapp(BuiltInFunc(Math Add),Var(['x'])),Literal(Int(1)))},Literal(Int(9)))
let simpleMod = (Funcapp(Funcapp(BuiltInFunc(Math Mod),Literal(Int(120))),Literal(Int(100))))
let simpleDiv = (Funcapp(Funcapp(BuiltInFunc(Math Div),simpleMod),Literal(Int(2))))
let test2 = Funcapp(Funcapp(BuiltInFunc(Math Add),simpleDiv),Literal(Int(3)))


let division = (Funcapp(Funcapp(BuiltInFunc(Math Div),Var(['b'])),Var(['a'])))
let addition = (Funcapp(Funcapp(BuiltInFunc(Math Add),Var(['a'])),division))
let ABbody = Lambda {InputVar = ['a']; Body = Lambda{InputVar = ['b'];Body = addition}}
let FBody = Funcapp(Funcapp(Var(['f']),Literal(Int(3))),Literal(Int(27)))

let test3 = Funcapp(Funcapp(ABbody,Literal(Int(3))),Literal(Int(27)))

let test4 = FuncDefExp(['f'],ABbody,FBody) 
// let f a b = a + b/a in f 3 27

let test5FBody = Funcapp(Funcapp(BuiltInFunc (Math Mult), Funcapp(Funcapp(BuiltInFunc (Math Mult), Var ['f']),Var['f'])),Var ['f'])
let test5 = FuncDefExp(['f'],Literal(Int(5)),test5FBody)
// let f = 2 in f*f*f

let test6FBody = Funcapp(Funcapp(Var(['f']),Literal(Int(3))),Funcapp(Funcapp(BuiltInFunc(Math Mult),Literal(Int(9))),Literal(Int(9))))
let test6 = FuncDefExp(['f'],ABbody,test6FBody) 
// let f a b = a + b/a in f 3 (9*9)

let test7GBody = Lambda{InputVar = ['x'];Body = Funcapp(Var['f'],Var ['x'])}
let test7FBody =Lambda{InputVar = ['a'];Body = Lambda{InputVar = ['b']; Body = Funcapp(Funcapp(BuiltInFunc (Math Add),Funcapp(Funcapp(BuiltInFunc (Math Mult), Var ['a']),Var['a'])),Var['b'])}} 
let test7 = FuncDefExp(['f'],test7FBody,FuncDefExp(['g'],test7GBody,(Funcapp(Var ['g'],Literal(Int 3)))))   
// let f a b = a*a + b in let g x = f x in g 3 

let test8 = FuncDefExp(['f'],Lambda{InputVar = ['x'];Body = Funcapp(Funcapp(BuiltInFunc (Math Add), Var ['x']),Literal (Int 1))},FuncDefExp(['g'],Lambda{InputVar = ['y']; Body = Funcapp (Funcapp (BuiltInFunc (Math Add), Var ['y']), Literal (Int 2))},(Funcapp(Var ['f'], Funcapp(Var['g'], Literal(Int 3))))))   
//let f x = x+1 in let g y = y+2 in g (f 3)

let test9 = FuncDefExp(['f'],Lambda{InputVar = ['x']; Body = Funcapp(Funcapp(BuiltInFunc (Math Add), Var ['x']),Literal (Int 1))},Funcapp (Var['f'],(Funcapp (Var ['f'], Literal(Int 3)))))  
// let f x = x+1 in f (f 3)

let o = Applicative

let result0 = exec o test0
let result1 = exec o test1
let result2 = exec o test2
let result3 = exec o test3
let result4 = exec o test4
let result5 = exec o test5
let result6 = exec o test6
let result7 = exec o test7
let result8 = exec o test8
let result9 = exec o test9

//let g a b = a*b*b in let f x y = (g x y)/2 in f 2 5

//let g a b = a+b 
//let f x = g x
//f 1

let f x = x+1
f (f 3)
