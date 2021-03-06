module CombinatorRuntimeModule
open System

// Type definition 
type MathType = 
   | Add 
   | Sub
   | Div
   | Mult
   | Mod

type BuiltInType = 
    | Mat of MathType
    | Explode | Implode 
    | PFst | PSnd | IsPair | P
    | Equal
    | True | False
    | BS | BK | BI

type AST = 
    | FuncDefExp of FuncDefExpType
    | Lambda of LambdaType
    | Var of char List
    | FuncApp of AST*AST
    | Pair of AST*AST
    | Literal of LitType
    | BFunc of BuiltInType
    | Null
    | Bracket of AST
    | Y
    | Lazy of AST

and FuncDefExpType = {
    Name: char list
    Body: AST
    Expression: AST
}

and LambdaType = {
    InputVar: char list
    Body: AST
}

and LitType = 
    | Int of int 
    | String of char list

//BRACKET ABSTRACTION
let rec BracketAbstract (lambda : Result<AST,string>) : Result<AST,string> =
    match lambda with
    | Ok (Lambda{InputVar = x; Body = FuncApp(E1,E2)})  ->
        let lambda1 = BracketAbstract (Ok (Lambda{InputVar = x; Body = E1}))
        let lambda2 = BracketAbstract (Ok (Lambda{InputVar = x; Body = E2}))
        match lambda1, lambda2 with
        | Error r, _ | _, Error r -> Error r
        | Ok xE1, Ok xE2 -> Ok <| FuncApp( FuncApp( BFunc BS, xE1), xE2)
    | Ok (Lambda{InputVar = x; Body = Pair(E1,E2)})  -> pairFunctionAbstract x E1 E2
    | Ok (Lambda{InputVar = x; Body = E1}) -> 
        if (Var x) = E1
        then Ok <| BFunc BI
        else Ok <| FuncApp(BFunc BK, E1)
    | _ -> sprintf "RUN-TIME ERROR : EXPECTED A LAMBDA EXPRESSION BUT GOT %A" lambda |> Error

and pairFunctionAbstract (x :char list) (E1:AST) (E2:AST) =
    match E1,E2 with
    | Literal _,_ | Null,_ -> FuncApp(BFunc BK, Pair(E1,E2)) |> Ok
    | _ , Null ->
        let absE1 = Lambda{InputVar = x; Body = E1} |> Ok |> Abstract
        match absE1 with
        | Error _ -> absE1
        | Ok E1 -> Pair(E1,Null) |> Ok
    | _ ->
        let absE1 = Lambda{InputVar = x; Body = E1} |> Ok |> Abstract
        let absE2 = Lambda{InputVar = x; Body = E2} |> Ok |> Abstract
        match absE1,absE2 with
        | Error r, _ | _, Error r -> Error r
        | Ok E1, Ok E2 -> Pair(E1,E2) |> Ok
        
and Abstract (E:Result<AST,string>) : Result<AST,string> =
    match E with 
    | Ok (FuncApp(E1,E2)) ->
        let aE1' = Abstract (Ok E1)
        let aE2' = Abstract (Ok E2)
        match aE1', aE2' with
        | Error r, _ | _, Error r -> Error r
        | Ok E1, Ok E2 -> Ok <| FuncApp( E1, E2)
    | Ok (Lambda{InputVar = x; Body = E1}) -> 
        let aE1' = Abstract (Ok E1)
        match aE1' with
        | Error r -> Error r
        | Ok E1 ->  Lambda{InputVar = x; Body = E1} |> Ok |> BracketAbstract
    | Ok (Pair(E1,E2)) ->
        let aE1' = Abstract (Ok E1)
        let aE2' = Abstract (Ok E2)
        match aE1', aE2' with
        | Error r, _ | _, Error r -> Error r
        | Ok E1, Ok E2 -> Ok <| Pair(E1, E2)
    | Ok (Lazy x) -> x |> Ok |> Abstract
    | Ok (Literal _)| Ok (BFunc _) | Ok Null | Ok (Var _) -> E
    | _ -> sprintf "RUN-TIME ERROR : EXPECTED AN AST FOR THE BRACKET ABSTRACTION BUT GOT %A" E |> Error

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
        | Error r, _ | _, Error r -> Error r
        | Ok reducE1', Ok reducE2' -> Pair (reducE1',reducE2') |> Ok
    | Ok (Lazy x) -> x |> Ok |> reducCombinator
    | Ok (Literal _) | Ok (Var _) | Ok (BFunc _) | Ok Null -> E
    | _ -> sprintf "RUN-TIME ERROR : EXPECTED AN AST FOR THE COMBINATOR REDUCTION BUT GOT %A" E |> Error

let explode (s:char list) = // Built-In function Explode and Implode
    let rec lstToASTLst (strLst:char list) = 
        match strLst with
        | [] -> Null
        | h :: tl -> Pair(Literal( String [h]), lstToASTLst tl)
    s |> lstToASTLst |> Ok
    
let rec implode (aSTLst:Result<AST,string>) =
    match aSTLst with
    | Ok (Pair(Literal( String x) , Pair(a,b))) -> 
        let y = implode (Ok (Pair(a,b)))
        match y with
        | Error r -> Error r
        | Ok (Literal( String y')) -> Ok <| Literal( String (List.append x y'))
        | _ -> sprintf "RUN-TIME ERROR : EXPECTED A STRING BUT USED %A" y |> Error
    | Ok (Pair(Literal( String a) , Null)) -> Ok <| Literal( String a)
    | _ ->  sprintf "RUN-TIME ERROR : EXPECTED A LIST BUT USED %A" aSTLst |> Error

let BuiltPair (op:BuiltInType) (x':Result<AST,string>) : Result<AST,string> = // FuncApp( BFunc op, x)
    match op, x' with
    | _, Error r -> Error r
    | PFst, Ok (Pair(a,_)) | PSnd, Ok (Pair(_,a)) -> Ok a
    | IsPair, Ok (Pair(_)) -> Ok <| BFunc True
    | IsPair, _ -> Ok <| BFunc False
    | Explode, Ok (Literal( String a)) -> explode a
    | Implode, Ok x -> x |> Ok |> implode
    | _ -> sprintf "RUN-TIME ERROR : EXPECTED A BuiltInFunction and used %A, or EXPECTED A Pair/List and used %A " op x' |> Error

let BuiltMath (func':MathType) (a':Result<AST,string>) (b':Result<AST,string>) : Result<AST,string> = // FuncApp( FuncApp( BFunc op, a), b)
    match func', a', b' with
    | _, Error r, _ | _, _, Error r -> Error r
    | Add, Ok (Literal(Int a)), Ok (Literal(Int b)) ->  Literal(Int (a+b)) |> Ok // a+b
    | Sub, Ok (Literal(Int a)), Ok (Literal(Int b)) ->  Literal(Int (a-b)) |> Ok // a-b
    | Mult, Ok (Literal(Int a)), Ok (Literal(Int b)) -> Literal(Int (a*b)) |> Ok // a*b
    | Div, Ok (Literal(Int a)), Ok (Literal(Int b)) ->  Literal(Int (a/b)) |> Ok // a/b
    | Mod, Ok (Literal(Int a)), Ok (Literal(Int b)) -> Literal(Int (((a%b)+b)%b)) |> Ok // a mod b
    | _ -> sprintf "RUN-TIME ERROR : EXPECTED A Int and got %A and %A " a' b' |> Error
    
let rec eval (x:Result<AST,string>) : Result<AST,string> =
    let reduceSKI =  x |> evalBracket |> reducCombinator
    match reduceSKI with
    | Error r -> Error r
    | Ok (FuncApp( BFunc op, x)) -> 
        let x' = x |> Ok |> eval
        BuiltPair op x'
    | Ok (FuncApp( FuncApp( func, a), b)) ->
        let func' = func |> Ok |> eval
        BuiltFF func' a b
    | Ok (Lazy x) -> x |> Ok |> eval
    | Ok (Literal _ ) | Ok (Pair(_,_)) | Ok Null | Ok (BFunc _) -> reduceSKI
    | _ -> sprintf "RUN-TIME ERROR : EXPECTED SKI AST BUT USED %A" reduceSKI |> Error

and evalBracket (x:Result<AST,string>) : Result<AST,string> =
    match x with
    | Error _ -> x
    | Ok (FuncApp( f, Pair(a,Null))) ->
        let evfa = FuncApp( f, a) |> Ok |> eval
        match evfa with
        | Error r-> Error r
        | Ok a-> Pair(a,Null) |> Ok
    | Ok (FuncApp( f, Pair(a,b))) ->
        let evfa = FuncApp( f, a) |> Ok |> eval
        let evfb = FuncApp( f, b) |> Ok |> eval
        match evfa, evfb with
        | Error r, _ | _, Error r-> Error r
        | Ok a, Ok b -> Pair(a,b) |> Ok
    | _ -> x
        
and BuiltFF (func':Result<AST,string>) (a':AST) (b':AST) : Result<AST,string> = // FuncApp( FuncApp( BFunc op, a), b)
    match func' with
    | Error r-> Error r
    | Ok (BFunc P) -> Pair (a',b') |> Ok //Pair
    | Ok (BFunc (Mat f)) -> //Arithmetic
        let a = a' |> Ok |> eval
        let b = b' |> Ok |> eval
        BuiltMath f a b
    | Ok (BFunc Equal) ->     //Equal
        if a' = b' then Ok (BFunc True) else Ok (BFunc False)
    | Ok (BFunc True) -> a' |> Ok |> eval  // Church Boolean
    | Ok (BFunc False) -> b' |> Ok |> eval // Church Boolean
    | _ -> sprintf "RUN-TIME ERROR : TYPE OF THE ARGUMENTS, USED %A and %A with %A" a' b' func' |> Error
    
let Reduce (y:Result<AST,string>) : Result<AST,string> =
    let rec reduceFuncTree (x:Result<AST,string>) : Result<AST,string> =
        match x with
        | Error _ -> x
        | Ok (FuncDefExp{Name =  f; Body = E1; Expression = E2}) ->
            let reduceE1 = E1 |> Ok |> reduceFuncTree
            let reduceE2 = E2 |> Ok |> reduceFuncTree
            match reduceE2 with 
            | Error _ -> reduceE2
            | Ok E2 ->
                let reducef = Lambda{InputVar = f; Body = E2} |> Ok |> reduceFuncTree
                match reduceE1,reducef with
                | Error r, _ | _, Error r -> Error r
                | Ok E1, Ok Ef -> FuncApp(Ef,E1) |> Ok |> reduceFuncTree
        | Ok (Lambda{InputVar = x; Body = E1}) ->
            (Lambda{InputVar = x; Body = E1}) |> Ok |> Abstract
        | Ok (FuncApp(E1,E2)) -> Ok (FuncApp(E1,E2))
        | Ok (Lazy x ) -> x |> Ok |> reduceFuncTree
        | Ok (Literal _ ) | Ok (Pair(_)) | Ok Null | Ok (BFunc _) -> x
        | _ -> sprintf "RUN-TIME ERROR : EXPECTED A RESULT<AST,STRING> BUT GOT %A"  x |> Error
    y |> reduceFuncTree |> eval
