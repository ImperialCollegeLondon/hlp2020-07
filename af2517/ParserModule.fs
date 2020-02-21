module ParserModule
type Bracket = Round |Square 
type Keyword = LET |RIGHTARROW |Extension of string 
type Token = 
    | Other of char list  
    | Bracket of Bracket
    | Keyword of Keyword
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
    | FuncDefExp of FuncDefExpType:AST //FuncApp(Lambda("f", 5*((Var f) 3)),Lambda("x",(Var x)+1)) needed?
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

let (|PITEM|_|) (tokLst: Result<Token list, Token list>)  =
    match tokLst with
    | Ok [] -> Error []
    | Ok (Other s :: rest) ->  Ok (Var s, rest)
    | Ok (IntToken s:: rest) -> Ok (Literal (Int s) , rest)
    | Ok (StringToken s::rest) -> Ok (Literal (String s), rest) //choose what to return 
    | Ok lst -> Error lst
    | Error lst -> Error lst
    |> Some

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
 

let rec BuildAppExp(inp: Result<Token list, Token list>):(AST* Result<Token list, Token list>) =
    match inp with
    | PITEM (Ok(s, lst)) -> match Ok lst with 
                                | PITEM (Ok(_, _)) ->  
                                    let result = BuildAppExp (Ok lst)
                                    (Funcapp(s, fst(result)), snd(result))
                                | _ -> (s, Ok lst) 
    | PITEM (Error lst) -> failwithf "Lst failed %A " lst
    | Error msg -> failwithf "What? %A" msg
    | Ok _ ->  failwithf "What? Can't happen" 
    | _ ->  failwithf "What? Can't happen"

let rec BuildMultExp (inp: Result<Token list, Token list>) (acc:Token list):(AST) = 
    match inp with  
    | Ok (hd::tl) when hd = Other ['*'] -> 
        let result = BuildAppExp (Ok acc)
                     |> fst
                     |> ExtractRightAppList []
                     |> List.rev
                     |> MakeLeftAppList
        Funcapp(Funcapp(BuiltInFunc Mult, result), BuildMultExp (Ok tl) [])
    | Ok (hd::tl) when hd = Other ['/'] -> 
        let result = BuildAppExp (Ok acc)
                     |> fst
                     |> ExtractRightAppList []
                     |> List.rev
                     |> MakeLeftAppList
        Funcapp(Funcapp(BuiltInFunc Div, result), BuildMultExp (Ok tl) [])
    | Ok (hd::tl) -> BuildMultExp (Ok tl) (acc @ [hd])
    | Ok [] -> 
            BuildAppExp (Ok acc)
           |> fst
           |> ExtractRightAppList []
           |> List.rev
           |> MakeLeftAppList
    | Error _ -> failwithf "what?"

let rec BuildAddExp (inp: Result<Token list, Token list>) (acc:Token list):(AST) = 
    match inp with  
    | Ok (hd::tl) when hd = Other ['+'] -> 
        let result = BuildMultExp (Ok acc) []
        Funcapp(Funcapp(BuiltInFunc Add, result), BuildAddExp (Ok tl) [] )
    | Ok (hd::tl) when hd = Other ['-'] -> 
        let result = BuildMultExp (Ok acc) []
        Funcapp(Funcapp(BuiltInFunc Sub, result), BuildAddExp (Ok tl) [] )
    | Ok (hd::tl) -> BuildAddExp (Ok tl) (acc @ [hd])
    | Ok [] -> 
        BuildMultExp (Ok acc) []
    | Error _ -> failwithf "what?"

let (|PMATCH|_|) (tok: Token) (tokLst: Result<Token list, Token list>) = 
    match tokLst with
    | Ok []  -> Error []
    | Ok (s :: rest) when s = tok -> Ok rest
    | Ok lst -> Error lst
    | Error lst -> Error lst
    |> Some