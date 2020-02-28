module Lambdas

type MathType = |Add|Sub|Mult|Div|Mod

type BuiltInType = 
    | Mat of MathType
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

let func_Def_Exp_to_Lambda fde = Funcapp(Lambda{InputVar = fde.Name; Body = fde.Expression},fde.Body)

let rec findValue (env:EnvironmentType) name=
    match env with
    | (n,v)::_ when n=name -> Ok v 
    | _::tl -> findValue tl name 
    | _ -> sprintf "Run-time error: %A is not defined" name |> Error

let execEqual x y = 
    match (x,y) with
    | (Literal(Int _),Literal(Int _)) 
    | (Literal(String _),Literal(String _))
    | Null,Null -> 
        if x = y then Ok trueAST else Ok falseAST
    | _ -> sprintf "Run-time error: Equal(%A , %A) is not a valid expression" x y |> Error //Error

let execMath op x y =
    match (x,y) with 
    |Literal (Int A),Literal(Int B) ->
        match op with
        | Add -> A+B |> Int |> Literal |> Ok //Literal(Int(valueA+valueB))
        | Sub -> A-B |> Int |> Literal |> Ok
        | Mult -> A*B |> Int |> Literal |> Ok
        | Div when B<>0 -> A/B |> Int |> Literal |> Ok
        | Mod when B<>0 -> A%B |> Int |> Literal |> Ok
        | Div | Mod when B = 0 -> "Run-time error: Cannot divide by 0!" |> Error
        | _ ->  "What? Shouldn't happen" |> Error
    | _ -> sprintf "Run-time error: %A(%A,%A) , is not a valid expression" op x y |> Error

let execPFst p = 
    match p with
    | Pair(a,_) -> Ok a
    | _ -> sprintf "Run-time error: PFst(%A) is not a valid expression" p |> Error

let execPSnd p = 
    match p with
    | Pair(_,b) -> Ok b
    | _ -> sprintf "Run-time error: PSnd(%A) is not a valid expression" p |> Error

let execIsPair x =
    match x with 
    | Pair(_) -> Ok trueAST 
    | _ -> Ok falseAST

let concat s1 s2 =
    match (s1,s2) with
    | (Literal(String(a)),Literal(String(b))) -> Literal(String(a@b)) 
    | _ -> printf "SHOULDNT HAPPEN"; Null

let rec execImplode lst = 
    match lst with
    | Null -> Literal(String([])) |> Ok
    | Pair(Literal(String([c])),snd) -> 
        match execImplode snd with
        | Ok stringTail -> (Literal(String [c]), stringTail) ||> concat |> Ok
        | Error err -> Error err
    | _ -> sprintf "%A is not a valid list to implode" lst |> Error


let rec execExplode (str) =
    match str with
    | (Literal (String [])) -> Ok Null
    | (Literal (String (hd::tl))) -> (Pair(Literal(String([hd])), Literal(String tl))) |> execExplode 
    | _ -> sprintf "Run-time error: %A is not a valid string to explode" str |> Error 

/////////EXEC THIS IS THE MAIN RUNTIME BODY
let rec exec (exp : AST) : Result<AST,string> =
    match exp with 
    | FuncDefExp(fde) -> fde |> func_Def_Exp_to_Lambda |> exec
    | Lazy(e) -> exec e
    | Funcapp (func,Lazy(arg)) -> 
        match exec func with 
        | Ok executedFunc -> Funcapp(executedFunc,Lazy(arg))  |> applyFunc
        | Error err -> Error err
    | Funcapp(func,arg) -> 
        match exec func with 
        | Ok executedFunc -> 
            match exec arg with 
            | Ok executedArg -> Funcapp(executedFunc,executedArg) |> applyFunc
            | Error err -> Error err
        | Error err -> Error err
    | Pair(a,b)-> evalPair (Pair(a,b))
    | Literal _ | BuiltInFunc _ | Null | Y | Var _ | Lambda _ -> exp |> Ok 

and applyFunc (exp:AST):Result<AST,string> = 
    match exp with
    | ONEARGFUN(Y,f) -> exec (Funcapp(f,Lazy(Funcapp(Y,f))))
    | ONEARGFUN(Lambda l,arg) ->
        match lookUp [(l.InputVar, arg)] l.Body with 
        | Ok(ast) -> exec ast
        | Error(e)-> Error(e)
    | TWOARGFUN (BuiltInFunc(P),arg1,arg2)-> Ok (Pair(arg1,arg2))
    | TWOARGFUN (BuiltInFunc(Equal),arg1,arg2) -> execEqual arg1 arg2 
    | TWOARGFUN (BuiltInFunc(Mat op), x, y) -> execMath op x y 
    | ONEARGFUN (BuiltInFunc(PFst),arg) -> execPFst arg 
    | ONEARGFUN (BuiltInFunc(PSnd),arg) -> execPSnd arg
    | ONEARGFUN (BuiltInFunc(IsPair),arg) -> execIsPair arg 
    | ONEARGFUN (BuiltInFunc(Implode),arg) -> 
        match evalPair arg with  
        | Ok pair -> execImplode pair 
        | Error err -> Error err 
    | ONEARGFUN (BuiltInFunc(Explode),arg) -> execExplode arg    
    | ONEARGFUN ((Literal(_)|Pair(_)|Null), _) -> sprintf "Run time error: %A is not a valid function application" (exp) |> Error
    | ONEARGFUN (BuiltInFunc _, _) -> exp |> Ok //this is to deal with double argument builtIn functions
    | _ -> Error("SHOULD NEVER HAPPEN")

and lookUp env exp = 
    match exp with
    | Var name -> findValue env name    
    | FuncDefExp fde -> fde |> func_Def_Exp_to_Lambda |> lookUp env
    | Funcapp(func,arg) -> 
        match (lookUp env func,lookUp env arg) with
        | (Ok lookedFunc, Ok lookedArg) -> Funcapp(lookedFunc, lookedArg) |> Ok
        | (Error err, _) | (_,Error err) -> Error err
    | Lazy(e) -> 
        match (lookUp env e) with 
        | Ok exp -> Ok (Lazy(exp))
        | Error err -> Error err
    | Pair(fst,snd) -> 
        match (lookUp env fst, lookUp env snd) with 
        | (Ok lookedFst, Ok lookedSnd) -> Pair(lookedFst, lookedSnd) |> Ok
        | (Error err, _) | (_,Error err)-> Error err
    | Lambda l ->
        let updatedEnv = (l.InputVar,Var l.InputVar)::env
        match lookUp updatedEnv l.Body with 
        | Ok updatedBody -> Ok (Lambda {InputVar = l.InputVar; Body = updatedBody})
        | Error err -> Error err
    | Literal _ | BuiltInFunc _ | Null | Y -> Ok exp

and evalPair exp =
    match exp with 
    | Pair(fst,snd)->
        match exec fst with
        | Ok fstResult -> 
            match exec snd with
            | Ok sndResult -> Ok(Pair(fstResult,sndResult))
            | Error err -> Error err
        | Error err -> Error err
    | _ -> sprintf "Run-time error: %A was expexted to be a pair" exp |> Error 

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


//concat (Literal(String['a';'b'])) (Literal(String ['c';'d']))
//execExplode (Literal(String ['a';'b';'c';'d']))
//execImplode (Pair(Literal (String ['a']),Pair(Literal (String ['b']),Pair (Literal (String ['c']),Pair (Literal (String ['d']),Null)))))


//let g a b = a*b*b in let f x y = (g x y)/2 in f 2 5

//let g a b = a+b 
//let f x = g x
//f 1

