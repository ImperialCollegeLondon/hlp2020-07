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

let execPFst p = 
    match p with
    | Pair(a,_) -> a
    | _ -> printfn "%A was expected to be a pair" p; Null //Error

let execPSnd p = 
    match p with
    | Pair(_,b) -> b
    | _ -> printfn "%A was expected to be a pair" p; Null //Error

let execIsPair x =
    match x with | Pair(_) -> trueAST | _ -> falseAST

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

let (|TWOARGFUN|_|) exp = 
    match exp with
    | Funcapp(Funcapp(func,x),y) -> Some (func, x, y)
    | _ -> None

let (|ONEARGFUN|_|) exp =
    match exp with
    | Funcapp(func,x) -> Some (func, x)
    | _ -> None 

let rec eval (env:EnvironmentType) exp =
    let evalPair env (Pair(a,b)) =
        let headResult = eval env a
        printf "%A" headResult
        Pair(headResult,eval env b)
        
    let applyFunc exp = 
        match exp with
        | ONEARGFUN (Lambda l,E) ->
            let updateEnv = (l.InputVar, eval env E)::env
            eval updateEnv l.Body 
        | TWOARGFUN (BuiltInFunc(Equal),arg1,arg2) -> execEqual arg1 arg2 
        | TWOARGFUN (BuiltInFunc(Math op), Literal x, Literal y) -> execMath op x y 
        | ONEARGFUN (BuiltInFunc(PFst),arg) -> execPFst arg 
        | ONEARGFUN (BuiltInFunc(PSnd),arg) -> execPSnd arg 
        | ONEARGFUN (BuiltInFunc(IsPair),arg) -> execIsPair arg 
        | ONEARGFUN (BuiltInFunc(Implode),arg) -> arg |> eval env |> execImplode  
        | ONEARGFUN (BuiltInFunc(Explode),arg) -> execExplode arg    
        | ONEARGFUN ((Literal(_)|Pair(_)|Null), _) -> printfn "Run time error: %A is not a valid function application" exp;Null 
        | _ -> exp //?

    match exp with  
    | FuncDefExp(name,body,E) -> eval env (Funcapp(Lambda{InputVar = name; Body = E},body))
    | TWOARGFUN(BuiltInFunc(P),arg1,arg2)-> evalPair env (Pair(arg1,arg2))
    | TWOARGFUN(chBool,a,_) when chBool = trueAST -> eval env a
    | TWOARGFUN(chBool,_,b) when chBool = falseAST -> eval env b
    | Var name  -> findValue env name 
    | Funcapp(ast1,ast2) -> Funcapp(eval env ast1,eval env ast2) |> applyFunc
    | Lambda l -> Lambda {InputVar = l.InputVar; Body = eval ((l.InputVar,Var l.InputVar)::env) l.Body}
    | Pair(a,b)-> evalPair env (Pair(a,b))
    | Literal(_) | BuiltInFunc(_) | Null -> exp
    //| x -> x 

let exec = eval [] //|> evaluate

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

let test10 = Funcapp(Funcapp(Funcapp(Funcapp(BuiltInFunc(Equal),Literal(Int 2)),Literal(Int 1)),test9),test8)
// if 2=1 then test9 else test8
let test11 = Funcapp(Funcapp(Funcapp(Funcapp(BuiltInFunc(Equal),Literal(Int 2)),Literal(Int 2)),test9),test8)
// if 2=2 then test9 else test8

let o = Applicative

let result0 = exec test0
let result1 = exec test1
let result2 = exec test2
let result3 = exec test3
let result4 = exec test4
let result5 = exec test5
let result6 = exec test6
let result7 = exec test7
let result8 = exec test8
let result9 = exec test9
let result10 = exec test10
let result11 = exec test11

concat (Literal(String['a';'b'])) (Literal(String ['c';'d']))
execExplode (Literal(String ['a';'b';'c';'d']))
execImplode (Pair(Literal (String ['a']),Pair(Literal (String ['b']),Pair (Literal (String ['c']),Pair (Literal (String ['d']),Null)))))


//let g a b = a*b*b in let f x y = (g x y)/2 in f 2 5

//let g a b = a+b 
//let f x = g x
//f 1

let f x = x+1
f (f 3)
