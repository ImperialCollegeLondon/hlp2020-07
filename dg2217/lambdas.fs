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
    | Y 
    | Lazy of AST
    | FuncDefExp of FuncDefExpType//:char list*AST*AST 
    | Lambda of LambdaType
    | Var of char list 
    | Funcapp of AST*AST
    | Pair of AST*AST 
    | Null 
    | Literal of LitType 
    | BuiltInFunc of BuiltInType

and LambdaType = {
    InputVar: char list
    Body: AST
}

and FuncDefExpType = {
    Name: char list;
    Body: AST
    Expression: AST
}

and LitType = 
    | Int of int 
    | String of char list 

and EnvironmentType = list<(char list)*AST>

let trueAST = Lambda {InputVar = ['x']; Body = Lambda {InputVar = ['y'];Body = Var ['x']}}
let falseAST = Lambda {InputVar = ['x']; Body = Lambda {InputVar = ['y']; Body = Var ['y']}}

let (|TWOARGFUN|_|) exp = 
    match exp with
    | Funcapp(Funcapp(func,x),y) -> Some (func, x, y)
    | _ -> None

let (|ONEARGFUN|_|) exp =
    match exp with
    | Funcapp(func,x) -> Some (func, x)
    | _ -> None 

let rec findValue (env:EnvironmentType) name=
    match env with
    | (n,v)::_ when n=name -> v
    | _::tl -> findValue tl name
    | _ -> printfn "Run-time error: %A is not defined" (Var name) ; Var(name)

let execEqual x y = 
    match (x,y) with
    | (Literal(Int _),Literal(Int _)) 
    | (Literal(String _),Literal(String _))
    | Null,Null -> 
        if x = y then trueAST else falseAST
    | (Var _,_)| (_,Var _) -> Funcapp(Funcapp(BuiltInFunc Equal,x),y)
    | _ -> printfn "Run-time error: Equal(%A , %A) is not a valid expression" x y;Null //Error

let execMath op x y =
    match (x,y) with 
    |Int A,Int B ->
        match op with
        | Add -> A+B |> Int |> Literal //Literal(Int(valueA+valueB))
        | Sub -> A-B |> Int |> Literal
        | Mult -> A*B |> Int |> Literal
        | Div when B<>0 -> A/B |> Int |> Literal
        | Mod when B<>0 -> A%B |> Int |> Literal
        | Div | Mod when B = 0 -> printfn "cannot divide by 0!"; Null
        | _ -> printf "What? Shouldn't happen" ; Null
    | _ -> printfn "Run-time error: %A(%A,%A) , is not a valid expression" op x y;Null//Funcapp(Funcapp(BuiltInFunc(Math op),x),y) //Error

let execPFst p = 
    match p with
    | Pair(a,_) -> a
    | _ -> printfn "Run-time error: PFst(%A) is not a valid expression" p; Null //Error

let execPSnd p = 
    match p with
    | Pair(_,b) -> b
    | _ -> printfn "Run-time error: PSnd(%A) is not a valid expression" p; Null //Error

let execIsPair x =
    match x with 
    | Pair(_) -> trueAST 
    | _ -> falseAST

let concat s1 s2 =
    match (s1,s2) with
    | (Literal(String(a)),Literal(String(b))) -> Literal(String(a@b))
    | _ -> printfn "Run-time error: concatenate(%A,%A) is not a valid expression" s1 s2 ; Null

let rec execImplode lst = 
    match lst with
    | Null -> Literal(String([]))
    | Pair(Literal(String([c])),p) -> (Literal(String [c]), (execImplode p)) ||> concat
    | _ -> printfn "Run-time error: %A is not a valid list" lst; lst //Error

let rec execExplode (str:AST) =
    match str with
    | Literal (String []) -> Null
    | Literal (String (hd::tl)) -> Pair(Literal(String([hd])), Literal(String tl) |> execExplode)
    | _ -> printfn "Run-time error: %A is not a valid string" str; str // Error 

/////////EXEC THIS IS THE MAIN RUNTIME BODY
let rec exec exp =
    match exp with 
    | FuncDefExp(fde) -> exec (Funcapp(Lambda{InputVar = fde.Name; Body = fde.Expression},fde.Body))
    | TWOARGFUN(BuiltInFunc(P),arg1,arg2)-> evalPair (Pair(arg1,arg2))
    | Lazy(e) -> exec e
    | Funcapp (func,Lazy(arg)) -> Funcapp(exec func, Lazy arg) |> applyFunc
    | Funcapp(func,arg) -> Funcapp(exec func,exec arg) |> applyFunc
    | Pair(a,b)-> evalPair (Pair(a,b))
    | Literal _ | BuiltInFunc _ | Null | Y | Var _ | Lambda _ -> exp

and applyFunc exp = 
    match exp with
    | ONEARGFUN (Y,f) -> 
        exec (Funcapp(f,Lazy(Funcapp(Y,f))))
    | ONEARGFUN (Lambda l,arg) ->
        lookUp [(l.InputVar, arg)] l.Body |> exec 
    | TWOARGFUN (BuiltInFunc(Equal),arg1,arg2) -> execEqual arg1 arg2 
    | TWOARGFUN (BuiltInFunc(Math op), Literal x, Literal y) -> execMath op x y 
    | ONEARGFUN (BuiltInFunc(PFst),arg) -> execPFst arg 
    | ONEARGFUN (BuiltInFunc(PSnd),arg) -> execPSnd arg 
    | ONEARGFUN (BuiltInFunc(IsPair),arg) -> execIsPair arg 
    | ONEARGFUN (BuiltInFunc(Implode),arg) -> arg |> exec |> execImplode  
    | ONEARGFUN (BuiltInFunc(Explode),arg) -> execExplode arg    
    | ONEARGFUN ((Literal(_)|Pair(_)|Null), _) -> printfn "Run time error: %A is not a valid function application" exp; exp 
    | ONEARGFUN (BuiltInFunc _, _) -> exp
    | _ -> printfn "Should NEVER happen"; Null 

and lookUp env exp = 
    match exp with
    | FuncDefExp fde -> lookUp env (Funcapp(Lambda{InputVar = fde.Name; Body = fde.Expression},fde.Body))
    | Funcapp(a,b) -> Funcapp(lookUp env a, lookUp env b)
    | Var name -> findValue env name
    | Lazy(e) -> Lazy(lookUp env e)
    | Pair(a,b) -> Pair(lookUp env a, lookUp env b)
    | Lambda l -> 
        let updatedEnv = (l.InputVar,Var l.InputVar)::env
        Lambda{InputVar = l.InputVar;Body = lookUp updatedEnv l.Body}
    | Literal _ | BuiltInFunc _ | Null | Y -> exp

and evalPair (Pair(a,b)) =
    let headResult = exec a
    printf "%A" headResult
    Pair(headResult,exec b)



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

let test4 = FuncDefExp{Name = ['f'];Body = ABbody;Expression = FBody}
// let f a b = a + b/a in f 3 27

let test5FBody = Funcapp(Funcapp(BuiltInFunc (Math Mult), Funcapp(Funcapp(BuiltInFunc (Math Mult), Var ['f']),Var['f'])),Var ['f'])
let test5 = FuncDefExp{Name = ['f'];Body = Literal(Int(5)); Expression = test5FBody}
// let f = 5 in f*f*f

let test6FBody = Funcapp(Funcapp(Var(['f']),Literal(Int(3))),Funcapp(Funcapp(BuiltInFunc(Math Mult),Literal(Int(9))),Literal(Int(9))))
let test6 = FuncDefExp{Name = ['f']; Body = ABbody; Expression =  test6FBody} 
// let f a b = a + b/a in f 3 (9*9)

let test7GBody = Lambda{InputVar = ['x'];Body = Funcapp(Var['f'],Var ['x'])}
let test7FBody =Lambda{InputVar = ['a'];Body = Lambda{InputVar = ['b']; Body = Funcapp(Funcapp(BuiltInFunc (Math Add),Funcapp(Funcapp(BuiltInFunc (Math Mult), Var ['a']),Var['a'])),Var['b'])}} 
let test7 = FuncDefExp{Name = ['f'];Body = test7FBody;Expression = FuncDefExp{Name = ['g'];Body = test7GBody;Expression = (Funcapp(Var ['g'],Literal(Int 3)))}}  
// let f a b = a*a + b in let g x = f x in g 3 

let test8 = FuncDefExp{Name = ['f']; Body = Lambda{InputVar = ['x'];Body = Funcapp(Funcapp(BuiltInFunc (Math Add), Var ['x']),Literal (Int 1))};Expression = FuncDefExp{Name = ['g'];Body = Lambda{InputVar = ['y']; Body = Funcapp (Funcapp (BuiltInFunc (Math Add), Var ['y']), Literal (Int 2))};Expression = (Funcapp(Var ['f'], Funcapp(Var['g'], Literal(Int 3))))}} 
//let f x = x+1 in let g y = y+2 in g (f 3)

let test9 = FuncDefExp{Name = ['f'];Body = Lambda{InputVar = ['x']; Body = Funcapp(Funcapp(BuiltInFunc (Math Add), Var ['x']),Literal (Int 1))};Expression = Funcapp (Var['f'],(Funcapp (Var ['f'], Literal(Int 3))))}  
// let f x = x+1 in f (f 3)

let test10 = Funcapp(Funcapp(Funcapp(Funcapp(BuiltInFunc(Equal),Literal(Int 2)),Literal(Int 1)),Lazy(test8)),Lazy(test9))
// if 2=1 then test9 else test8
let test11 = Funcapp(Funcapp(Funcapp(Funcapp(BuiltInFunc(Equal),Literal(Int 2)),Literal(Int 2)),Lazy(test8)),Lazy(test9))
// if 2=2 then test9 else test8

let elseBody12 = Funcapp(Funcapp(BuiltInFunc(Math Mult),Var ['n']),Funcapp(Var ['f'], Funcapp(Funcapp(BuiltInFunc(Math Sub),Var ['n']),Literal(Int 1))))
let ifStatement12 = Funcapp(Funcapp(Funcapp(Funcapp(BuiltInFunc Equal,Var ['n']),Literal(Int 0)),Lazy(Literal(Int 1))),Lazy(elseBody12))
let fBody12 = Lambda{InputVar = ['f']; Body = Lambda{InputVar = ['n']; Body = ifStatement12}}
let test12 = FuncDefExp{Name = ['f'];Body = fBody12;Expression = Funcapp(Funcapp(Var ['f'],Lazy(Funcapp(Y,Var['f']))),Literal(Int 5))}
// let rec f n = if n = 0 then 1 else n*f(n-1) in f 2 SAME AS let f' f n = if n = 0 then 1 else n*f(n-1) in f' (Y h) 2 

let fminus1 = Funcapp(Var ['f'], Funcapp(Funcapp(BuiltInFunc (Math Sub), Var ['a']),Literal (Int 1)))
let fminus2 = Funcapp(Var ['f'], Funcapp(Funcapp(BuiltInFunc (Math Sub), Var ['a']),Literal (Int 2)))
let recBody = Funcapp(Funcapp(BuiltInFunc (Math Add), fminus1),fminus2)
let eqBody1 = Funcapp(Funcapp(BuiltInFunc Equal, Var ['a']), Literal(Int 1))
let eqBody0 = Funcapp(Funcapp(BuiltInFunc Equal, Var ['a']), Literal(Int 0))
let ifelseBody13 = Funcapp(Funcapp(eqBody0,Lazy(Literal(Int 0))),Lazy(Funcapp(Funcapp(eqBody1,Lazy(Literal(Int 1))),Lazy(recBody)))) 
let test13 = 
    FuncDefExp{
        Name = ['f'];
        Body = Lambda{InputVar = ['f']; Body = Lambda {InputVar = ['a']; Body = ifelseBody13}};
        Expression = Funcapp(Funcapp(Var ['f'],Lazy(Funcapp(Y,Var['f']))),Literal(Int 25))} 
//fibonacci
//let rec f a = if a = 0 then 0 else if a = 1 then 1 else f(a-1) + f(a-2) in f 10

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
let result12 = exec test12
let result13 = exec test13
(*let vladT = (FuncDefExp
   { Name = ['f']
     Body =
           Lambda
             { InputVar = ['x']
               Body =
                     Funcapp
                       (Funcapp (BuiltInFunc (Math Mult),Var ['x']),
                        Literal (Int 2)) }
     Expression = Funcapp (Var ['f'],Literal (Int 2)) })
let result13 = exec vladT*)

//let f x y = x * y * 2 + 1 in let g y = y in f 2 (g 5)

//let result14 = exec test14

concat (Literal(String['a';'b'])) (Literal(String ['c';'d']))
execExplode (Literal(String ['a';'b';'c';'d']))
execImplode (Pair(Literal (String ['a']),Pair(Literal (String ['b']),Pair (Literal (String ['c']),Pair (Literal (String ['d']),Null)))))


//let g a b = a*b*b in let f x y = (g x y)/2 in f 2 5

//let g a b = a+b 
//let f x = g x
//f 1

let f x = x+1
f (f 3)

let rec f n = if n = 0 then 1 else n*f(n-1) in f 4