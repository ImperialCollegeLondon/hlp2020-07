module CombinatorRuntimeModule
open System
open Definitions
// Type definition 

//BRACKET ABSTRACTION
let rec BracketAbstract (lambda : Result<AST,string>) : Result<AST,string> =
    match lambda with
    | Ok (Lambda{InputVar = x; Body = FuncApp(E1,E2)})  ->
        let lambda1 = BracketAbstract (Ok (Lambda{InputVar = x; Body = E1}))
        let lambda2 = BracketAbstract (Ok (Lambda{InputVar = x; Body = E2}))
        match lambda1, lambda2 with
        | Error r, _ | _, Error r -> Error r
        | Ok xE1, Ok xE2 -> Ok <| FuncApp( FuncApp( BFunc BS, xE1), xE2)
    | Ok (Lambda{InputVar = x; Body = E1}) -> 
        if (Var x) = E1
        then Ok <| BFunc BI
        else Ok <| FuncApp(BFunc BK, E1)
    | _ -> sprintf "RUN-TIME ERROR : EXPECTED A LAMBDA EXPRESSION BUT GOT %A" lambda |> Error

let rec Abstract (E:Result<AST,string>) : Result<AST,string> =
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
        | Ok E1 -> BracketAbstract <| Ok (Lambda{InputVar = x; Body = E1})
    | Ok (Pair(E1,E2)) ->
        let aE1' = Abstract (Ok E1)
        let aE2' = Abstract (Ok E2)
        match aE1', aE2' with
        | Error r, _ | _, Error r -> Error r
        | Ok E1, Ok E2 -> Ok <| Pair(E1, E2)
    | Ok (Lzy x) -> x |> Ok |> Abstract // Extension
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
    | Ok (Lzy x) -> x |> Ok |> reducCombinator //Extension
    | Ok (Literal _) | Ok (Var _) | Ok (BFunc _) | Ok Null -> E
    | _ -> sprintf "RUN-TIME ERROR : EXPECTED AN AST FOR THE COMBINATOR REDUCTION BUT GOT %A" E |> Error


let explode (s:char list) = // Built-In function Explode and Implode
    let rec lstToASTLst (strLst:char list) = 
        match strLst with
        | [] -> Null
        | h :: tl -> Pair(Literal( Str [h]), lstToASTLst tl)
    s |> lstToASTLst |> Ok
    
let rec implode (aSTLst:Result<AST,string>) =
    match aSTLst with
    | Ok (Pair(Literal( Str x) , Pair(a,b))) -> 
        let y = implode (Ok (Pair(a,b)))
        match y with
        | Error r -> Error r
        | Ok (Literal( Str y')) -> Ok <| Literal( Str (List.append x y'))
        | _ -> sprintf "RUN-TIME ERROR : EXPECTED A STRING BUT USED %A" y |> Error
    | Ok (Pair(Literal( Str a) , Null)) -> Ok <| Literal( Str a)
    | _ ->  sprintf "RUN-TIME ERROR : EXPECTED A LIST BUT USED %A" aSTLst |> Error

let BuiltMathBool (func':Result<AST,string>) (a':Result<AST,string>) (b':Result<AST,string>) : Result<AST,string> = // FuncApp( FuncApp( BFunc op, a), b)
    match func', a', b' with
    //Arithmetic
    | Error r, _, _ | _, Error r, _ | _, _, Error r -> Error r
    | Ok (BFunc (Mat Add)), Ok (Literal(Int a)), Ok (Literal(Int b)) ->  Literal(Int (a+b)) |> Ok // a+b
    | Ok (BFunc (Mat Sub)), Ok (Literal(Int a)), Ok (Literal(Int b)) ->  Literal(Int (a-b)) |> Ok // a-b
    | Ok (BFunc (Mat Mult)), Ok (Literal(Int a)), Ok (Literal(Int b)) -> Literal(Int (a*b)) |> Ok // a*b
    | Ok (BFunc (Mat Div)), Ok (Literal(Int a)), Ok (Literal(Int b)) ->  Literal(Int (a/b)) |> Ok // a/b
    | Ok (BFunc (Mat Mod)), Ok (Literal(Int a)), Ok (Literal(Int b)) -> Literal(Int (((a%b)+b)%b)) |> Ok // a mod b
    //Equal
    | Ok (BFunc Equal), a, b -> 
        if a = b then Ok (BFunc True) else Ok (BFunc False)
    // Church Boolean
    | Ok (BFunc True), a, _ | Ok (BFunc False), _, a -> a
    | _ -> sprintf "RUN-TIME ERROR : TYPE OF THE ARGUMENTS, USED %A and %A with %A" a' b' func' |> Error

let BuiltPair (op:BuiltInType) (x':Result<AST,string>) : Result<AST,string> = // FuncApp( BFunc op, x)
    match op, x' with
    | _, Error r -> Error r
    | PFst, Ok (Pair(a,_)) | PSnd, Ok (Pair(_,a)) -> Ok a
    | IsPair, Ok (Pair(_)) -> Ok <| BFunc True
    | IsPair, _ -> Ok <| BFunc False
    | Explode, Ok (Literal( Str a)) -> explode a
    | Implode, Ok x -> implode (Ok x)
    | _ -> sprintf "RUN-TIME ERROR : EXPECTED A BuiltInFunction and used %A, or EXPECTED A Pair/List and used %A " op x' |> Error

let rec eval (x:Result<AST,string>) : Result<AST,string> = 
    let reduceSKI = reducCombinator x
    match reduceSKI with
    | Error r -> Error r
    | Ok (FuncApp( FuncApp( BFunc P, a), b)) -> Pair (a,b) |> Ok
    | Ok (FuncApp( BFunc op, x)) -> 
        let x' = x |> Ok |> eval
        BuiltPair op x'
    | Ok (FuncApp( FuncApp( func, a), b)) ->
        let a' = a |> Ok |> eval
        let b' = b |> Ok |> eval
        let func' = func |> Ok |> eval
        BuiltMathBool func' a' b'
    | Ok (Lzy x) -> x |> Ok |> eval // Extension
    | Ok (Literal _ ) | Ok (Pair(_,_)) | Ok Null | Ok (BFunc _) -> reduceSKI
    | _ -> sprintf "RUN-TIME ERROR : EXPECTED SKI AST BUT USED %A" reduceSKI |> Error
    
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
        | Ok (Lzy x ) -> x |> Ok |> reduceFuncTree //Extension
        | Ok (Literal _ ) | Ok (Pair(_)) | Ok Null | Ok (BFunc _) -> x
        | _ -> sprintf "RUN-TIME ERROR : EXPECTED A RESULT<AST,STRING> BUT GOT %A"  x |> Error
    y |> reduceFuncTree |> eval