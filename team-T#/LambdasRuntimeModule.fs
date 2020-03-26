module LambdasRuntimeModule
open Expecto
open Definitions
open System

type  EnvironmentType = list<(char list)*AST>

let trueAST = Lambda {InputVar = ['x']; Body = Lambda {InputVar = ['y'];Body = Var ['x']}}
let falseAST = Lambda {InputVar = ['x']; Body = Lambda {InputVar = ['y']; Body = Var ['y']}}

let unitAST = UNIT |> Literal |> Ok

let bindEmptyVariables (pairs:AST) : EnvironmentType =
    let rec bindEmptyVariablesHelper(pairs: AST) (acc:EnvironmentType) : EnvironmentType =
        match pairs with
            |Pair (Var x, Null) | ExactPairMatch (Var x, Null) -> acc @ [x, Var x]
            |Pair (Literal _, Null) | ExactPairMatch (Literal _, Null) -> acc
            |Pair (Var x,y) | ExactPairMatch (Var x,y) -> bindEmptyVariablesHelper y (acc @ [x, Var x])
            |Pair (Literal _, y) | ExactPairMatch (Literal _, y) -> bindEmptyVariablesHelper y acc
            |_  -> []
    
    bindEmptyVariablesHelper pairs []

let firstASTOccurrence (x:AST) (lst:(AST*AST) list ) : int =
    let rec firstASTOccurrenceHelper (lst:(AST*AST) list) (acc:int) : int =
        match lst with
            |hd::_ when (fst hd) = x -> acc
            |_::tl -> firstASTOccurrenceHelper tl (acc+1)
            |[] -> -1
    firstASTOccurrenceHelper lst 0 
  
let rec bindPairHelper (matchPattern:AST) (casePattern:AST) (acc : (AST * AST) list )  : ((AST * AST) list) option=
        match matchPattern,casePattern with
            |Null,Null -> Some acc
            |Null, Pair (Var x, Null) -> Some <| acc @ [Null, Var x]
            |Null, Pair (Var x, y) -> None
            |Null, ExactPairMatch _ -> None
            |_,Null -> None
            
            | Pair (Literal x1, Null), ExactPairMatch (Literal x2, Null) when x1 = x2 -> Some acc 
            | Pair (Literal x1, Null), ExactPairMatch (Var x2, Null) -> Some <| acc @ [Literal x1, Var x2]
            | Pair (Literal x1, A), ExactPairMatch (Literal x2, B) -> bindPairHelper A B acc
            | Pair (Literal x1, A), ExactPairMatch (Var x2, B) -> bindPairHelper A B (acc @ [Literal x1, Var x2])
             
             
            |Pair(Literal x1, y), Pair(Literal x2, z) when x1 = x2 -> bindPairHelper y z acc
            |Pair(Literal x1, y), Pair(Var x2,z) when z <> Null -> bindPairHelper y z (acc @ [Literal x1, Var x2])
            |Pair(Literal x1,y), Pair(Var x2,Null) -> Some <| acc @ [Pair(Literal x1,y), Var x2]
            
            
            |Pair(Literal x1,_),Pair(Literal x2,_) when x1 <> x2 -> None
            | x,y -> print x; print y; failwithf "Why is this happening in bindPairHelper"       
 
let rec bindPair (toMatch: AST) (intoThis:AST list) : ((AST * AST) list) * int =
        
    let rec bindPairIndex (y:AST list) (index:int)  : ((AST * AST) list) * int =
        match y with
        |hd::tl ->
            match bindPairHelper toMatch hd [] with
                |Some x -> x,index
                |None -> bindPairIndex tl (index+1)
        |[] -> failwithf "no case matched - provide default case that matches all"
    
    bindPairIndex intoThis 0
    

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
        | Greater -> if A>B then trueAST |> Ok else falseAST |> Ok
        | Lower -> if A<B then trueAST |> Ok else falseAST |> Ok
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
    | _ -> sprintf "Run-time error: Wrong argument given to implode" |> Error

let rec execExplode str =
    match str with
    | (Literal (Str [])) -> Ok Null
    | (Literal (Str (hd::tl))) -> 
        match execExplode (Literal(Str tl)) with 
        | Ok(explodedTail) -> Pair(Literal(Str [hd]), explodedTail) |> Ok
        | Error(err)-> Error(err)  
    | _ -> sprintf "Run-time error: Wrong argument given to explode" |> Error 


let rec pairToList p = 
    match p with 
    | Null -> Ok []
    | Pair(a,b) -> 
        match (a, (pairToList b)) with
        | _, Error(err) -> Error(err)
        | Null, Ok(bLst) -> string []::bLst |> Ok
        | Literal(Int n), Ok(bLst) -> string(n)::bLst |> Ok
        | Literal(Str cLst), Ok(bLst) -> String.Concat(Array.ofList(cLst)) :: bLst |> Ok
        | Pair(_), Ok(bLst) -> 
            match pairToList a with
            | Ok(aLst) -> string aLst :: bLst |> Ok
            | Error(err)->Error(err)
        | _ -> sprintf "Run-time error: Non valid item was attempted to be printed"|> Error
    | _ -> sprintf "Run-time error: Non valid item was attempted to be printed" |> Error

let execPrint x = 
    match x with
    | _ when x = trueAST -> print "true" ; unitAST
    | _ when x = falseAST -> print "false" ; unitAST
    | Literal(Int n) ->  print n ; unitAST
    | Literal(Str cLst) -> String.Concat(Array.ofList(cLst)) |> print ; unitAST
    | Null -> print "[]" ; unitAST
    | Pair(a, b) -> 
        match pairToList (Pair(a,b)) with
        | Ok(listP) -> listP |> print ; UNIT |> Literal |> Ok
        | Error(err)-> Error(err)
    | _ -> sprintf "Run-time error: Non valid item was attempted to be printed" |> Error

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

let matchReducer (x:AST*AST) : (char list*AST) = 
    match x with
        | y, Var name -> name,y
        | _ -> failwithf "should be a variable. Anything else not supported yet in match"

/////////EXEC IS THE MAIN RUNTIME BODY
let rec exec (exp : AST) : Result<AST,string> =
    match exp with
    | MatchDef f ->
        match exec f.Condition with
        |Ok (Literal x)  ->
            //type of f.Cases -> (AST * AST) list
            //type exec f.Condition  -> Result<AST,string>
            let occ = firstASTOccurrence (Literal x) f.Cases
            exec <| snd f.Cases.[occ]
        //Assume only pairs can be matched so far
        |Ok x ->
            let binded,caseNum =  bindPair x (List.map fst f.Cases)
            // HERE DANI
            let binded = List.map matchReducer binded
            let dani2 = snd f.Cases.[caseNum]
            match lookUp (binded @ globalEnv) dani2 with
                    | Ok(lookedUp) -> exec lookedUp
                    | Error(err)->Error(err)
        |Error x -> failwithf "Condition from match is not a valid expression"
                           
    | FuncDefExp(fde) -> fde |> func_Def_Exp_to_Lambda |> exec
    | MutFuncDef(namesList, bodiesList) -> createRecBundle (namesList,bodiesList) |> exec
    | FuncDef(name, body) -> 
        match exec body with
        | Ok(result) -> globalEnv <- (name, result)::globalEnv ; Ok(result)
        | Error err -> Error err
    | Lzy(lazyExp) -> exec lazyExp
    | FuncApp(func, arg) -> memoise execFunc (func,arg)
    | Pair(a,b)-> evalPair (Pair(a,b))
    | Var(name) -> 
        match (findValue globalEnv name) with
        | Ok(result) -> exec result
        | Error(err) -> Error(err)
    | Literal _ | BFunc _ | Null | Y | Lambda _ -> exp |> Ok 

and execFunc (func,arg) = 
    match arg with 
    | Lzy(lazyArg) -> 
        match exec func with 
        | Ok executedFunc -> FuncApp(executedFunc,Lzy(lazyArg))  |> applyBasicFunc
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
    | ONEARGFUN(Y,f) -> exec (FuncApp(f,Lzy(FuncApp(Y,f))))
    | ONEARGFUN(Lambda l,arg) ->
        match lookUp ((l.InputVar, arg)::globalEnv) l.Body with 
        | Ok(ast) -> exec ast
        | Error(e)-> Error(e)
    | TWOARGFUN (BFunc(P),arg1,arg2) -> evalPair (Pair(arg1,arg2))
    | TWOARGFUN (BFunc(Equal),arg1,arg2) -> evalIfLzy2ARG execEqual arg1 arg2 
    | TWOARGFUN (BFunc(Mat op), arg1, arg2) -> evalIfLzy2ARG (execMath op) arg1 arg2 
    | ONEARGFUN (BFunc(PFst),arg) -> evalIfLzy execPFst arg
    | ONEARGFUN (BFunc(PSnd),arg) ->  evalIfLzy execPSnd arg
    | ONEARGFUN (BFunc(IsPair),arg) -> evalIfLzy execIsPair arg 
    | ONEARGFUN (BFunc(Implode),arg) -> evalIfLzy execImplode arg
    | ONEARGFUN (BFunc(Explode),arg) -> evalIfLzy execExplode arg  
    | ONEARGFUN (BFunc(Print),arg) -> evalIfLzy execPrint arg  
    | ONEARGFUN ((Literal(_)|Pair(_)|Null), _) -> sprintf "Run time error: %A is not a valid function application" (exp) |> Error
    | ONEARGFUN (BFunc _, _) -> exp |> Ok //this is to deal with double argument builtIn functions
    | _ -> Error(sprintf "SHOULD NEVER HAPPEN: %A" exp) 

and lookUp (env:EnvironmentType) exp = 
    match exp with
    | MatchDef f ->
        //print env
        match lookUp env f.Condition with        
        | Ok(lookedUpCondition) ->
            let res = List.map (
                                   (function |x,Ok y -> x,y |_ -> failwithf "Looking up one of the matches failed" )
                                   <<
                                   (fun (caseHeadUnbindedVars,bindedVars,caseBody) -> caseHeadUnbindedVars, lookUp (bindedVars @ env) caseBody )
                                   <<
                                   //this one comes first
                                   (fun (caseHeadUnbindedVars,caseBody) ->
                                      // printf "caseHeadUnbindedVars: %A\n caseBody: %A\n bindedVars: %A \n" caseHeadUnbindedVars caseBody (bindEmptyVariables caseHeadUnbindedVars)
                                       caseHeadUnbindedVars,bindEmptyVariables caseHeadUnbindedVars,caseBody )
                               ) f.Cases
            //lookUp (newEnv @ env) body
            Ok <| MatchDef {Condition = lookedUpCondition; Cases =  res }           
        | Error(err) -> Error(err)
        
        //failwithf "not supported yet"
    | Var name -> findValue env name
    | FuncDefExp fde -> fde |> func_Def_Exp_to_Lambda |> lookUp env
    | FuncApp(func,arg) -> 
        match (lookUp env func,lookUp env arg) with
        | (Ok lookedFunc, Ok lookedArg) -> FuncApp(lookedFunc, lookedArg) |> Ok
        | (Error err, _) | (_,Error err) -> Error err
    | Lzy(e) -> 
        match (lookUp env e) with 
        | Ok exp -> Ok (Lzy(exp))
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

and evalIfLzy func arg : Result<AST,string> = 
    match exec arg with
    | Ok res -> func res
    | Error err -> Error err

and evalIfLzy2ARG func arg1 arg2 : Result<AST,string> = 
    match (exec arg1, exec arg2) with 
    | Ok res1, Ok res2 -> func res1 res2
    | Error err, _ | _, Error err -> Error err

let run input =
    match input with 
    | Error(err)-> Error(err)
    | Ok(exp)-> exec exp   


////////TESTS/////////

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

let test9 = FuncDefExp{Name = ['f'];Body = Lambda{InputVar = ['x']; Body = FuncApp(FuncApp(BFunc (Mat Add), Var ['x']),Literal (Int 1L))};Expression = FuncApp (Var['f'],(FuncApp (Var ['f'], Literal(Str ['a']))))}  
// let f x = x+1 in f (f "a")

let test10 = FuncApp(FuncApp(FuncApp(FuncApp(BFunc(Equal),Literal(Int 2L)),Literal(Int 1L)),Lzy(test8)),Lzy(test9))
// if 2=1 then test9 else test8
let test11 = FuncApp(FuncApp(FuncApp(FuncApp(BFunc(Equal),Literal(Int 2L)),Literal(Int 2L)),Lzy(test8)),Lzy(test9))
// if 2=2 then test9 else test8

let elseBody12 = FuncApp(FuncApp(BFunc(Mat Mult),Var ['n']),FuncApp(Var ['f'], FuncApp(FuncApp(BFunc(Mat Sub),Var ['n']),Literal(Int 1L))))
let ifStatement12 = FuncApp(FuncApp(FuncApp(FuncApp(BFunc Equal,Var ['n']),Literal(Int 0L)),Lzy(Literal(Int 1L))),Lzy(elseBody12))
let fBody12 = Lambda{InputVar = ['f']; Body = Lambda{InputVar = ['n']; Body = ifStatement12}}
let test12 = FuncDefExp{Name = ['f'];Body = fBody12;Expression = FuncApp(FuncApp(Var ['f'],Lzy(FuncApp(Y,Var['f']))),Literal(Int 5L))}
// let rec f n = if n = 0 then 1 else n*f(n-1) in f 2 SAME AS let f' f n = if n = 0 then 1 else n*f(n-1) in f' (Y h) 2 

let fminus1 = FuncApp(Var ['f'], FuncApp(FuncApp(BFunc (Mat Sub), Var ['a']),Literal (Int 1L)))
let fminus2 = FuncApp(Var ['f'], FuncApp(FuncApp(BFunc (Mat Sub), Var ['a']),Literal (Int 2L)))
let recBody = FuncApp(FuncApp(BFunc (Mat Add), fminus1),fminus2)
let eqBody1 = FuncApp(FuncApp(BFunc Equal, Var ['a']), Literal(Int 1L))
let eqBody0 = FuncApp(FuncApp(BFunc Equal, Var ['a']), Literal(Int 0L))
let ifelseBody13 = FuncApp(FuncApp(eqBody0,Lzy(Literal(Int 0L))),Lzy(FuncApp(FuncApp(eqBody1,Lzy(Literal(Int 1L))),Lzy(recBody)))) 
let test13 = 
    FuncDefExp{
        Name = ['f'];
        Body = Lambda{InputVar = ['f']; Body = Lambda {InputVar = ['a']; Body = ifelseBody13}};
        Expression = FuncApp(FuncApp(Var ['f'],Lzy(FuncApp(Y,Var['f']))),Literal(Int 25L))} 
//fibonacci
//let rec f a = if a = 0 then 0 else if a = 1 then 1 else f(a-1) + f(a-2) in f 92

let tailBody14 = FuncApp(Var ['f'], FuncApp(BFunc (PSnd), Var ['p']))
let headBody14 = FuncApp(FuncApp(BFunc (Mat Mult),FuncApp(BFunc PFst,(Var['p']))),Literal (Int 2L))
let elseBody14 = FuncApp(FuncApp(BFunc P, Lzy(headBody14)),Lzy(tailBody14))
let eqBody14 = FuncApp(FuncApp(BFunc Equal, Var ['p']), Null)
let ifelseBody14 = FuncApp(FuncApp(eqBody14,Lzy(Null)),Lzy(elseBody14)) 
let list14 = Pair(Literal(Int 1L), Pair(FuncApp(FuncApp(BFunc(Mat Mult), Literal(Int 3L)),Literal(Int 2L)), Pair(Literal(Int 2L), Pair(test12, Null))))
let test14 =
    FuncDefExp{
        Name = ['f'];
        Body = Lambda{InputVar = ['f']; Body = Lambda {InputVar = ['p']; Body = ifelseBody14}};
        Expression = FuncApp(FuncApp(Var ['f'],Lzy(FuncApp(Y,Var['f']))),list14)}  
//let rec f p = if p = [] then [] else  ((p.Head)*2)::f(p.Tail) in f [1;2*3;3;test12]

let test_List_Lambda_Runtime =
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
            Expect.equal (exec test4) (Error """Run-time error: "n" is not defined""") "f a b = a+n/b in f 4 36"        
        }
        test "Runtime Test 5" {
            Expect.equal (exec test5) (Ok(Literal (Int 125L))) "let f = 5 in f*f*f"        
        }
        test "Runtime Test 6" {
            Expect.equal (exec test6) (Error "Run-time error: Wrong argument given to implode") "let f a b = a + b/a in f 3 (Implode 9 9)"        
        }
        test "Runtime Test 7" {
            Expect.equal (exec test7) (Ok(Lambda { InputVar = ['b']; Body = FuncApp(FuncApp(BFunc (Mat Add),FuncApp(FuncApp (BFunc (Mat Mult),Literal (Int 3L)),Literal (Int 3L))),Var ['b'])})) "let f a b = a*a + b in let g x = f x in g 3 "      
        }
        test "Runtime Test 8" {
            Expect.equal (exec test8) (Ok(Literal (Int 6L))) "let f x = x+1 in let g y = y+2 in g (f 3)"        
        }
        test "Runtime Test 9" {
            Expect.equal (exec test9) (Error "Run-time error: Add(Literal (Str ['a']),Literal (Int 1L)) , is not a valid expression") """let f x = x+1 in f (f "a")"""        
        }
        test "Runtime Test 10" {
            Expect.equal (exec test10) (Error "Run-time error: Add(Literal (Str ['a']),Literal (Int 1L)) , is not a valid expression") "if 2=1 then test9 else test8"        
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

let testsWithExpectoLambdaRuntime() =
    runTests defaultConfig test_List_Lambda_Runtime |> ignore

