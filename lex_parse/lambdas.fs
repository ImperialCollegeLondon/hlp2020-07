module Lambdas
open Definitions

type  EnvironmentType = list<(char list)*AST>

let trueAST = Lambda {InputVar = ['x']; Body = Lambda {InputVar = ['y'];Body = Var ['x']}}
let falseAST = Lambda {InputVar = ['x']; Body = Lambda {InputVar = ['y']; Body = Var ['y']}}

let (|TWOARGFUN|_|) exp = 
    match exp with
    | FuncApp(FuncApp(func,x),y) -> Some (func, x, y)
    | _ -> None

let (|ONEARGFUN|_|) exp =
    match exp with
    | FuncApp(func,x) -> Some (func, x)
    | _ -> None 

let func_Def_Exp_to_Lambda fde = FuncApp(Lambda{InputVar = fde.Name; Body = fde.Expression},fde.Body)

let rec findValue (env:EnvironmentType) name=
    match env with
    | (n,v)::_ when n=name -> Ok v 
    | _::tl -> findValue tl name 
    | _ -> name |> List.map string |> List.reduce (+) |> sprintf "Run-time error: %A is not defined"|> Error

let execEqual x y = 
    match (x,y) with
    | (Literal(Int _),Literal(Int _)) 
    | (Literal(Str _),Literal(Str _))
    | Null,Null -> 
        if x = y then Ok trueAST else Ok falseAST
    | Pair(_), Null | Null, Pair(_) -> Ok falseAST
    | _ -> sprintf "Run-time error: Equal(%A , %A) is not a valid expression" x y |> Error //Error

let execMath op x y =
    match (x,y) with 
    |Literal (Int A),Literal(Int B) ->
        match op with
        | Add -> A+B |> Int |> Literal |> Ok //Literal(Int(valueA+valueB))
        | Sub -> A-B |> Int |> Literal |> Ok
        | Mult -> A*B |> Int |> Literal |> Ok
        | Div when B<>0L -> A/B |> Int |> Literal |> Ok
        | Mod when B<>0L -> A%B |> Int |> Literal |> Ok
        | Div | Mod when B = 0L -> "Run-time error: Cannot divide by 0!" |> Error
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
    | (Literal(Str(a)),Literal(Str(b))) -> Literal(Str(a@b)) 
    | _ -> printf "SHOULDNT HAPPEN"; Null

let rec execImplode lst = 
    match lst with
    | Null -> Literal(Str([])) |> Ok
    | Pair(Literal(Str([c])),snd) -> 
        match execImplode snd with
        | Ok stringTail -> (Literal(Str [c]), stringTail) ||> concat |> Ok
        | Error err -> Error err
    | _ -> sprintf "Run-time error: %A is not a valid list to implode" lst |> Error

let rec execExplode (str) =
    match str with
    | (Literal (Str [])) -> Ok Null
    | (Literal (Str (hd::tl))) -> (Pair(Literal(Str([hd])), Literal(Str tl))) |> execExplode 
    | _ -> sprintf "Run-time error: %A is not a valid string to explode" str |> Error 


let mutable cache = Map []

let mutable globalEnv:EnvironmentType =  [] 

let memoise fn =
   fun x ->
      match Map.containsKey x cache with
      | true -> cache.[x] // return cached value
      | false -> let res = fn x // compute function
                 cache <- Map.add x res cache //store result in cache
                 res // return result

let createRecBundle (namesList, bodiesList) : AST = 
    let toPairNames (res:EnvironmentType) (el:char list) =
        match res with 
        |(_, (FuncApp(BFunc PFst, p)))::_ -> (el,FuncApp(BFunc PFst , (FuncApp(BFunc PSnd, p))))::res
        |[] -> [(el, FuncApp(BFunc PFst, (Var [' '])))]
        | _ -> printf "%A SHOULD NEVER HAPPEN" res ; []
    let newNamesEnv = namesList |> List.fold toPairNames []
    globalEnv <- newNamesEnv @ globalEnv

    let toNestedPairs res el =
        match res with
        | Null -> Pair(el,Null)
        | _ -> Pair(el, res)
    (FuncDef( [' '], FuncApp(Y,Lambda{InputVar = [' ']; Body = bodiesList |> List.rev |> List.fold toNestedPairs Null})))


/////////EXEC IS THE MAIN RUNTIME BODY
let rec exec (exp : AST) : Result<AST,string> =
    match exp with 
    | FuncDefExp(fde) -> fde |> func_Def_Exp_to_Lambda |> exec
    | MutFuncDef(namesList, bodiesList) -> createRecBundle (namesList,bodiesList) |> exec
    | FuncDef(name, body) -> 
        match exec body with
        | Ok(result) -> globalEnv <- (name, result)::globalEnv ; Ok(result)
        | Error err -> Error err
    | Lazy(lazyExp) -> exec lazyExp
    | FuncApp(func, arg) -> memoise execFunc (func,arg)
    | Pair(a,b)-> evalPair (Pair(a,b))
    | Var(name) -> 
        match (findValue globalEnv name) with
        | Ok(result) -> exec result
        | Error(err) -> Error(err)
    | Literal _ | BFunc _ | Null | Y | Lambda _ -> exp |> Ok 

and execFunc (func,arg) = 
    match arg with 
    | Lazy(lazyArg) -> 
        match exec func with 
        | Ok executedFunc -> FuncApp(executedFunc,Lazy(lazyArg))  |> applyBasicFunc
        | Error err -> Error err
    | _ -> 
        match exec func with 
        | Ok executedFunc -> 
            match exec arg with 
            | Ok executedArg -> FuncApp(executedFunc,executedArg) |> applyBasicFunc
            | Error err -> Error err
        | Error err -> Error err

and applyBasicFunc (exp:AST):Result<AST,string> = 
    match exp with
    | ONEARGFUN(Y,f) -> exec (FuncApp(f,Lazy(FuncApp(Y,f))))
    | ONEARGFUN(Lambda l,arg) ->
        match lookUp ((l.InputVar, arg)::globalEnv) l.Body with 
        | Ok(ast) -> exec ast
        | Error(e)-> Error(e)
    | TWOARGFUN (BFunc(P),arg1,arg2) -> evalPair (Pair(arg1,arg2))
    | TWOARGFUN (BFunc(Equal),arg1,arg2) -> evalIfLazy2ARG execEqual arg1 arg2 
    | TWOARGFUN (BFunc(Mat op), arg1, arg2) -> evalIfLazy2ARG (execMath op) arg1 arg2 
    | ONEARGFUN (BFunc(PFst),arg) -> evalIfLazy execPFst arg
    | ONEARGFUN (BFunc(PSnd),arg) ->  evalIfLazy execPSnd arg
    | ONEARGFUN (BFunc(IsPair),arg) -> evalIfLazy execIsPair arg 
    | ONEARGFUN (BFunc(Implode),arg) -> evalIfLazy execImplode arg
    | ONEARGFUN (BFunc(Explode),arg) -> evalIfLazy execExplode arg    
    | ONEARGFUN ((Literal(_)|Pair(_)|Null), _) -> sprintf "Run time error: %A is not a valid function application" (exp) |> Error
    | ONEARGFUN (BFunc _, _) -> exp |> Ok //this is to deal with double argument builtIn functions
    | _ -> Error(sprintf "SHOULD NEVER HAPPEN: %A" exp) 

and lookUp (env:EnvironmentType) exp = 
    match exp with
    | Var name -> findValue env name
    | FuncDefExp fde -> fde |> func_Def_Exp_to_Lambda |> lookUp env
    | FuncApp(func,arg) -> 
        match (lookUp env func,lookUp env arg) with
        | (Ok lookedFunc, Ok lookedArg) -> FuncApp(lookedFunc, lookedArg) |> Ok
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
    | Literal _ | BFunc _ | Null | Y -> Ok exp

and evalPair exp =
    match exp with 
    | Pair(fst,snd)  ->
        match exec fst with
        | Ok fstResult -> 
            match exec snd with
            | Ok sndResult -> Ok(Pair(fstResult,sndResult))
            | Error err -> Error err
        | Error err -> Error err
    | _ -> sprintf "Run-time error: %A was expexted to be a pair" exp |> Error 

and evalIfLazy func arg : Result<AST,string> = 
    match exec arg with
    | Ok res -> func res
    | Error err -> Error err

and evalIfLazy2ARG func arg1 arg2 : Result<AST,string> = 
    match (exec arg1, exec arg2) with 
    | Ok res1, Ok res2 -> func res1 res2
    | Error err, _ | _, Error err -> Error err

let run input =
    match input with 
    | Error(err)-> Error(err)
    | Ok(exp)-> exec exp   

let elseBody12 = FuncApp(FuncApp(BFunc(Mat Mult),Var ['n']),FuncApp(Var ['f'], FuncApp(FuncApp(BFunc(Mat Sub),Var ['n']),Literal(Int 1L))))
let ifStatement12 = FuncApp(FuncApp(FuncApp(FuncApp(BFunc Equal,Var ['n']),Literal(Int 0L)),Lazy(Literal(Int 1L))),Lazy(elseBody12))
let fBody12 = FuncApp(Y, Lambda{InputVar = ['f']; Body = Lambda{InputVar = ['n']; Body = ifStatement12}})
let test12 = FuncDefExp{Name = ['f'];Body = fBody12;Expression = FuncApp(Var ['f'],Literal(Int 5L))}

let result12 = exec  test12

let namesList = [['a'];['b']]
let bodyA = (FuncApp (FuncApp(FuncApp (FuncApp (BFunc Equal,Var ['n']),Literal (Int 0L)),Lazy (Literal (Int 1L))),Lazy(FuncApp(Var ['b'],FuncApp (FuncApp (BFunc (Mat Sub),Var ['n']),Literal (Int 1L))))))
let bodyB = (FuncApp (FuncApp(FuncApp (FuncApp (BFunc Equal,Var ['n']),Literal (Int 0L)),Lazy (Literal (Int 0L))),Lazy(FuncApp(Var ['a'],FuncApp (FuncApp (BFunc (Mat Sub),Var ['n']),Literal (Int 1L))))))
let bodiesList = [Lambda{InputVar = ['n']; Body = bodyA};Lambda{InputVar = ['n']; Body = bodyB}]

let test15 = MutFuncDef(namesList, bodiesList)
let result15 = exec test15
exec (FuncApp(Var ['a'], Literal(Int 70L)))
