module CombinatorRuntimeModule
open System

// Type definition 
type BuiltInType = 
    | BPlus | BMinus | BTimes | BDivide | BMod
    | BExplode | BImplode 
    | BPFst | BPSnd | BIsPair | BP //creates a pair 
    | BEqual //works for strings ints and nulls 
    | BTrue | BFalse | BIfThenElse
    | BS | BK | BI

type AST = 
    | FuncDefExp of FunDefExpType:(char list) * AST * AST
    | Lambda of LambdaType:(char list) * Body:AST
    | Var of char List
    | FuncApp of AST*AST
    | Pair of AST*AST
    | Literal of LitType
    | BFunc of BuiltInType
    | NULL

and LitType = 
    | IntLit of int 
    | StringLit of string

//BRACKET ABSTRACTION
let rec BracketAbstract (lambda : Result<AST,string>) : Result<AST,string> =
    match lambda with
    | Ok (Lambda(x, FuncApp(E1,E2)))  ->
        let lambda1 = BracketAbstract (Ok (Lambda(x, E1)))
        let lambda2 = BracketAbstract (Ok (Lambda(x, E2)))
        match lambda1, lambda2 with
        | Error r, _ -> Error r
        | _, Error r -> Error r
        | Ok xE1, Ok xE2 -> Ok <| FuncApp( FuncApp( BFunc BS, xE1), xE2)
    | Ok (Lambda(x, E1)) -> 
        if (Var x) = E1
        then Ok <| BFunc BI
        else Ok <| FuncApp(BFunc BK, E1)
    | _ -> Error "RUN-TIME ERROR IN THE BRACKETABSTRACTION"

let rec Abstract (E:Result<AST,string>) : Result<AST,string> =
    match E with 
    | Ok (FuncApp(E1,E2)) ->
        let aE1' = Abstract (Ok E1)
        let aE2' = Abstract (Ok E2)
        match aE1', aE2' with
        | Error r, _ -> Error r
        | _, Error r -> Error r
        | Ok E1, Ok E2 -> Ok <| FuncApp( E1, E2)
    | Ok (Lambda( x, E1)) -> 
        let aE1' = Abstract (Ok E1)
        match aE1' with
        | Error r -> Error r
        | Ok E1 -> BracketAbstract <| Ok (Lambda( x, E1))
    | Ok (Pair(E1,E2)) ->
        let aE1' = Abstract (Ok E1)
        let aE2' = Abstract (Ok E2)
        match aE1', aE2' with
        | Error r, _ -> Error r
        | _, Error r -> Error r
        | Ok E1, Ok E2 -> Ok <| Pair(E1, E2)
    | Ok (Literal a) -> Ok <| Literal a
    | Ok (BFunc op) -> Ok (BFunc op)
    | Ok NULL -> Ok NULL
    | Ok (Var f) -> Ok (Var f)
    | _ -> Error "RUN-TIME ERROR IN THE ABSTRACT"

let rec reducCombinator (E:Result<AST,string>) : Result<AST,string> = // Reduction of the SKI combinator
    match E with 
    | Ok (FuncApp( FuncApp( FuncApp( BFunc BS, f), g), x)) ->
        FuncApp( FuncApp( f, x), FuncApp( g, x)) |> Ok |> reducCombinator
    | Ok (FuncApp( FuncApp( BFunc BK, x), _)) ->  x |> Ok |> reducCombinator
    | Ok (FuncApp( BFunc BI, x)) -> x |> Ok |> reducCombinator
    | Ok (FuncApp(E1,E2)) ->
        let reducE1 = E1 |> Ok |> reducCombinator
        let reducE2 = E2 |> Ok |> reducCombinator
        match reducE1, reducE2 with
        | Error r, _ -> Error r
        | _, Error r -> Error r
        | Ok reducE1', Ok reducE2' ->
            if reducE1 = Ok E1
            then FuncApp(E1, reducE2') |> Ok
            else FuncApp(reducE1', reducE2') |> Ok |> reducCombinator
    | Ok (Pair(E1,E2)) ->
        let reducE1 = E1 |> Ok |> reducCombinator
        let reducE2 = E2 |> Ok |> reducCombinator
        match reducE1, reducE2 with
        | Error r, _ -> Error r
        | _, Error r -> Error r
        | Ok reducE1', Ok reducE2' -> Pair (reducE1',reducE2') |> Ok
    | Ok (Literal x) -> Literal x |> Ok
    | Ok (Var x) -> Var x |> Ok
    | Ok (BFunc f) -> BFunc f |> Ok
    | Ok NULL -> NULL |> Ok
    | _ -> Error "RUN-TIME ERROR IN EVAL COMBINATOR"

//BUILT-IN FUNCTION
let explode (s:string) = // Built-In function BExplode and BImplode
    let rec lstToASTLst strLst = 
        match strLst with
        | [] -> NULL
        | h :: tl -> Pair(Literal( StringLit h), lstToASTLst tl)
    s |> Seq.toList |> List.map Char.ToString |> lstToASTLst |> Ok

let rec implode (aSTLst:Result<AST,string>) =
    match aSTLst with
    | Ok (Pair(Literal( StringLit x) , Pair(a,b))) -> 
        let y = implode (Ok (Pair(a,b)))
        match y with
        | Error r -> Error r
        | Ok (Literal( StringLit y')) -> Ok <| Literal( StringLit (x+y'))
        | _ -> Error "RUN-TIME ERROR : EXPECTED A STRING"
    | Ok (Pair(Literal( StringLit a) , NULL)) -> Ok <| Literal( StringLit a)
    | _ -> Error "RUN-TIME ERROR : EXPECTED A LIST"    

let BuiltMathBool (func':Result<AST,string>) (a':Result<AST,string>) (b':Result<AST,string>) : Result<AST,string> = // FuncApp( FuncApp( BFunc op, a), b)
    match func', a', b' with
    //Arithmetic
    | Error r, _, _ -> Error r
    | _, Error r, _ -> Error r
    | _, _, Error r -> Error r
    | Ok (BFunc BPlus), Ok (Literal(IntLit a)), Ok (Literal(IntLit b)) -> Ok (Literal(IntLit (a+b))) // a+b
    | Ok (BFunc BMinus), Ok (Literal(IntLit a)), Ok (Literal(IntLit b)) ->  Ok (Literal(IntLit (a-b))) // a-b
    | Ok (BFunc BTimes), Ok (Literal(IntLit a)), Ok (Literal(IntLit b)) -> Ok (Literal(IntLit (a*b))) // a*b
    | Ok (BFunc BDivide), Ok (Literal(IntLit a)), Ok (Literal(IntLit b)) -> Ok (Literal(IntLit (a/b))) // a/b
    | Ok (BFunc BMod), Ok (Literal(IntLit a)), Ok (Literal(IntLit b)) -> Ok (Literal(IntLit (((a%b)+b)%b))) // a mod b
    //Bequal
    | Ok (BFunc BEqual), a, b -> 
        if a = b then Ok (BFunc BTrue) else Ok (BFunc BFalse)
    // Church Boolean
    | Ok (BFunc BTrue), a, _ -> a
    | Ok (BFunc BFalse), _, b -> b
    | _ -> Error "RUN-TIME ERROR IN BuiltMathBool"

let BuiltPair (op:BuiltInType) (x':Result<AST,string>) : Result<AST,string> = // FuncApp( BFunc op, x)
    match op, x' with
    | _, Error r -> Error r
    | BPFst, Ok (Pair(a,_)) -> Ok a
    | BPSnd, Ok (Pair(_,b)) -> Ok b
    | BIsPair, Ok (Pair(_)) -> Ok <| BFunc BTrue
    | BIsPair, _ -> Ok <| BFunc BFalse
    | BExplode, Ok (Literal( StringLit a)) -> explode a
    | BImplode, Ok x -> implode (Ok x)
    | _ -> Error "RUN-TIME ERROR IN BuiltPair"

// EVALUATION PHASE
let rec eval (x:Result<AST,string>) : Result<AST,string> = 
    let reduceSKI = reducCombinator x
    match reduceSKI with
    | Error r -> Error r
    | Ok (FuncApp( FuncApp( FuncApp( BFunc BIfThenElse, a), b), c)) -> 
        let evalA = a |> Ok |> eval
        match evalA with
        | Error r -> Error r
        | Ok (BFunc BTrue) -> b |> Ok |> eval
        | Ok (BFunc BFalse) -> c |> Ok |> eval
        | _ -> Error "RUN-TIME ERROR : A BOOLEAN WAS EXPECTED BUT WE GOT A ..."
    | Ok (FuncApp( FuncApp( BFunc BP, a), b)) -> Pair (a,b) |> Ok
    | Ok (FuncApp( BFunc op, x)) -> 
        let x' = x |> Ok |> eval
        BuiltPair op x'
    | Ok (FuncApp( FuncApp( func, a), b)) ->
        let a' = a |> Ok |> eval
        let b' = b |> Ok |> eval
        let func' = func |> Ok |> eval
        BuiltMathBool func' a' b'
    | Ok (Literal a )-> Literal a |> Ok
    | Ok (Pair(a,b)) -> Pair(a,b) |> Ok
    | Ok NULL -> Ok NULL
    | Ok (BFunc f) -> BFunc f |> Ok
    | _ -> Error "RUN-TIME ERROR IN THE EVALUATION PHASE"

let rec substitudeVarWithExpression (f: char list) (e1: AST) (e2: Result<AST,string>) : Result<AST,string> =
    match e2 with
    | Ok (Pair(A,B)) ->
        let a = substitudeVarWithExpression f e1 (Ok A)
        let b = substitudeVarWithExpression f e1 (Ok B)
        match a, b with
        | Error r, _ -> Error r
        | _, Error r -> Error r
        | Ok a', Ok b' -> Pair(a',b') |> Ok
    | Ok (FuncApp( e21, e22)) ->
        let e21' = substitudeVarWithExpression f e1 (Ok e21)
        let e22' = substitudeVarWithExpression f e1 (Ok e22)
        match e21', e22' with
        | Error r, _ -> Error r
        | _, Error r -> Error r
        | Ok e21, Ok e22 ->
            if e21 = Var f
            then FuncApp( e1, e22) |> Ok
            else FuncApp( e21, e22) |> Ok
    | Ok E ->
        if E = Var f
        then e1 |> Ok
        else 
            match E with
            | Var g -> Var g |> Ok
            | Literal a -> Literal a |> Ok
            | BFunc op -> BFunc op |> Ok
            | NULL -> NULL |> Ok
            | _ -> Error "RUN-TIME ERROR IN substitudeVarWithExpression 1"
    | _ -> Error "RUN-TIME ERROR IN substitudeVarWithExpression 2"

let reductionAST (y:AST) : Result<AST,string> =
    let rec reduceFuncTree (x:Result<AST,string>) : Result<AST,string> =
        match x with
        | Ok (FuncDefExp( f, E1, E2)) ->
            let evalE1 = E1 |> Ok |> reduceFuncTree
            let evalE2 = E2 |> Ok |> reduceFuncTree
            match evalE1 with 
            | Ok evalE1' -> 
                substitudeVarWithExpression f evalE1' evalE2
            | _ -> Error "RUN-TIME ERROR IN reductionAST 1"
        | Ok (Lambda( x, E1)) ->
            (Lambda( x, E1)) |> Ok |> Abstract
        | Ok (FuncApp(E1,E2)) -> Ok (FuncApp(E1,E2))
        | _ -> Error "RUN-TIME ERROR IN reductionAST 2"
    y |> Ok |> reduceFuncTree |> eval

