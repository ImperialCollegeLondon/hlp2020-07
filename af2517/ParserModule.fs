module ParserModule
type Bracket = Round |Square 
type Keywords = LET |RIGHTARROW |Extension of string |EQUAL |IN
type Token = 
    | Other of char list  
    | Bracket of char list
    | Keyword of Keywords
    | IntToken of int  
    | StringToken of char list 

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
    | FuncDefExp of FuncDefExpType:(char list * AST)*AST 
    | Lambda of LambdaType:(char list)*Body:AST
    | Var of char list //only valid in lambdas 
    | Funcapp of AST*AST
    | Pair of AST*AST 
    | Null 
    | Literal of LitType 
    | BuiltInFunc of BuiltInType

and LitType = 
    | Int of int 
    | String of char list 
    | True of AST //make it a named function with Lambda("A",Lambda ("B",Var "A" ))
    | False of AST 

    
let rec ExtractRightAppList (lst:AST list) (inp:AST) : AST list = 
// extracts the top-level right-associative application list and returns it as an Fsharp list
    match inp with 
    | Funcapp (hd, tl) -> lst @ [hd] @ (ExtractRightAppList lst tl)
    | Var el ->  lst @ [Var el]
    | _ -> failwithf "What? Shouldn't happen"
    
let rec MakeLeftAppList (inp:AST list) : AST =
// takes an Fsharp list and makes a left associative FuncApp tree representing the items
    match inp with 
    | [el] ->  el
    | hd::tl -> Funcapp (MakeLeftAppList tl, hd)
    | [] -> failwithf "What? Can't happen"
     

let (|PMATCH|_|) (tok: Token) (tokLst: Result<Token list, Token list>) = 
    match tokLst with
    | _ when tok = Bracket [')'] -> None
    | Ok []  -> None
    | Ok (s :: rest) when s = tok -> Some(Ok rest)
    | Ok lst -> None
    | Error lst -> None
    


let rec (|PITEM|_|) (tokLst: Result<Token list, Token list>)  =
    match tokLst with
    | Ok [] -> Error []
    | Ok (Other s :: rest) ->  Ok (Var s, Ok rest)
    | Ok (IntToken s:: rest) -> Ok (Literal (Int s) , Ok rest)
    | Ok (StringToken s::rest) -> Ok (Literal (String s), Ok rest)
    | PMATCH (Bracket ['(']) (PBUILDADDEXP(ast, PMATCH (Bracket [')']) (inp'))) -> Ok (ast, inp') 
    | Ok lst -> failwithf "Pmatch Not matching properly %A" lst
    | Error lst ->  failwithf "Pmatch Not matching properly %A" lst
    |> Some


and BuildAppExp(inp: Result<Token list, Token list>):(AST* Result<Token list, Token list>) =
    match inp with
    | PITEM (Ok(s, lst)) -> match lst with 
                                  | Ok [] -> (s, lst)
                                  | Ok _ ->  
                                      let result = BuildAppExp (lst)
                                      (Funcapp(s, fst(result)), snd(result))
                                  | _ -> failwithf "What? Can't happen for now "
                                 
                 
    | PITEM (Error lst) -> failwithf "Lst failed %A " lst
    | Error msg -> failwithf "What? %A" msg
    | Ok _ ->  failwithf "What? Can't happen" 
    | _ ->  failwithf "What? Can't happen"

and BuildMultExp (inp: Result<Token list, Token list>) (acc:Token list):(AST* Result<Token list, Token list>) = 
    match inp with  
    | Ok (hd::tl) when hd = Other ['*'] -> 
        let result = BuildAppExp (Ok acc)
                     |> fst
                     |> ExtractRightAppList []
                     |> List.rev
                     |> MakeLeftAppList
        (Funcapp(Funcapp(BuiltInFunc Mult, result), fst(BuildMultExp (Ok tl) [])), snd (BuildAppExp (Ok acc)))
    | Ok (hd::tl) when hd = Other ['/'] -> 
        let result = BuildAppExp (Ok acc)
                     |> fst
                     |> ExtractRightAppList []
                     |> List.rev
                     |> MakeLeftAppList
        (Funcapp(Funcapp(BuiltInFunc Div, result), fst(BuildMultExp (Ok tl) [])), snd (BuildAppExp (Ok acc)))
    | Ok (hd::tl) -> BuildMultExp (Ok tl) (acc @ [hd])
    | Ok [] -> 
           let res = BuildAppExp (Ok acc)
                     |> fst
                     |> ExtractRightAppList []
                     |> List.rev
                     |> MakeLeftAppList
           (res, Ok [])
    | Error _ -> failwithf "what?"

and BuildAddExp  (acc:Token list) (inp: Result<Token list, Token list>):(AST* Result<Token list, Token list>) = 
    match inp with  
    | Ok (hd::tl) when hd = Other ['+'] -> 
        let MultResult =  BuildMultExp (Ok acc) []
        let AddResult = BuildAddExp [] (Ok tl)
        (Funcapp(Funcapp(BuiltInFunc Add, fst MultResult), fst AddResult ), snd MultResult )
    | Ok (hd::tl) when hd = Other ['-'] -> 
        let MultResult =  BuildMultExp (Ok acc) []
        let AddResult = BuildAddExp [] (Ok tl) 
        (Funcapp(Funcapp(BuiltInFunc Sub, fst MultResult), fst AddResult), snd MultResult)
    | Ok (hd::tl) -> BuildAddExp (acc @ [hd]) (Ok tl) 
    | Ok [] -> 
        BuildMultExp (Ok acc) []
    | Error lst -> failwithf "what %A?" lst

and (|PBUILDADDEXP|_|) (inp: Result<Token list, Token list>) = 
    Some (BuildAddExp [] inp)


and BuildLambda inp = 
    match inp with 
    | hd::(hd'::tl) -> match hd,hd' with 
                        | (Other x),(Other y) -> Lambda(x, BuildLambda tl)
                        | (Other x), (Keyword EQUAL) -> Lambda(x, fst(BuildAddExp [] (Ok tl)))
                        | (Keyword EQUAL), _ ->  fst(BuildAddExp [] (Ok tl)) //let f = 3 for example
                        | _ -> failwithf "Invalid arguments"
    | _ -> failwithf "insufficient expression"

//FunctionDef (("f", Lambda(x, Var"x"+1)), f 3)
//FunctionDef(("f", Lambda(x,Lambda(y,Var x + Var y)), f 2 3) Buildlambda x BuildLambda
//function definition consists of let "name" "params" "equals" "exp" "in" "exp (could be another function definition)"
and ExtractParts inp acc = 
    match inp with 
    | hd::tl when hd = Keyword IN -> acc,tl
    | hd::tl -> ExtractParts tl (acc @ [hd])
    | [] -> failwithf "No expression evaluated"
    

and BuildFunctionDef inp  = 
    match inp with 
    | hd::tl  -> match hd with 
                 | Other x -> 
                    let body,expression = ExtractParts tl []
                    FuncDefExp ((x, BuildLambda body), CheckForLet (Ok expression))
                 | _ -> failwithf "Not a valid function name"
    | _ -> failwithf "insufficient expression LET X"

and CheckForLet (inp: Result<Token list, Token list>)  = 
    match inp with
    | PMATCH (Keyword LET) (Ok rest) -> BuildFunctionDef (rest) 
    | _ -> fst(BuildAddExp [] inp)
