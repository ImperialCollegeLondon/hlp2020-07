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
    | FuncDefExp of FuncDefExpType:AST //FuncApp(Lambda("f", x*(f x)),Lambda("x",x+1))
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

type AppExp = AppExpItem of AST | AppExpExp of AST*AppExp
type MultExp = NoMult of AppExp|MultApp of AppExp*MultExp
type AddExp = NoSum of MultExp | AddApp of MultExp*AddExp  
type ASTBRACKETS = BRA of ASTBRACKETS | EXP of char list

let (|PMATCH|_|) (tok: Token) (tokLst: Result<Token list, Token list>) = 
    match tokLst with
    | Ok []  -> Error []
    | Ok (s :: rest) when s = tok -> Ok rest
    | Ok lst -> Error lst
    | Error lst -> Error lst
    |> Some

let (|PITEM|_|) (tokLst: Result<Token list, Token list>)  =
    match tokLst with
    | Ok [] -> Error []
    | Ok (Other s :: rest) ->  Ok (Var s, rest)
    | Ok (IntToken s:: rest) -> Ok (Literal (Int s) , rest)
    | Ok (StringToken s::rest) -> Ok (Literal (String s), rest) //choose what to return 
    | Ok lst -> Error lst
    | Error lst -> Error lst
    |> Some

let (|PMULT|_|) (tokLst: Result<Token list, Token list>) = 
    match tokLst with
    | Ok [] -> Error []
    | Ok (Other ['*'] :: rest) ->  Ok rest
    | Ok lst -> Error lst
    | Error lst -> Error lst
    |> Some

//one recursive function needed for every precedence level 

let rec BuildAppExp(inp: Result<Token list, Token list>):(AppExp* Result<Token list, Token list>) =
    match inp with
    | PITEM (Ok(s, lst)) -> match Ok lst with 
                                | PITEM (Ok(_, _)) ->  
                                    let result = BuildAppExp (Ok lst)
                                    (AppExpExp(s, fst(result)), snd(result))
                                | _ -> ((AppExpItem s), Ok lst) 
    | PITEM (Error lst) -> failwithf "Lst failed %A " lst
    | Error msg -> failwithf "What? %A" msg
    | Ok _ ->  failwithf "What? Can't happen" 
    | _ ->  failwithf "What? Can't happen"
        

let rec FlattenAST (lst:AST list) (inp:AppExp)  = 
    match inp with 
    | AppExpExp (hd, tl) -> (FlattenAST lst tl) @ [hd] @ lst
    | AppExpItem el -> [el] @ lst

let rec ReverseAST (inp: AST list): AppExp = 
    match inp with 
    | [el] -> AppExpItem el
    | hd::tl -> AppExpExp (hd, ReverseAST tl)
    | [] -> failwithf "What? Can't happen"

let rec BuildMultExp(inp: Result<Token list, Token list>) (acc:Token list):(MultExp) = 
    match inp with  
    | Ok (hd::tl) when hd = Other ['*'] -> 
        let result = BuildAppExp (Ok acc)
                     |> fst
                     |> (FlattenAST [])
                     |> ReverseAST
        MultApp(result, BuildMultExp (Ok tl) [])
    | Ok (hd::tl) -> BuildMultExp (Ok tl) (acc @ [hd])
    | Ok [] -> 
        let result = BuildAppExp (Ok acc)
                     |> fst
                     |> (FlattenAST [])
                     |> ReverseAST 
        NoMult result
    | Error _ -> failwithf "what?"


let rec BuildAddExp(inp: Result<Token list, Token list>) (acc:Token list):(AddExp) = 
    match inp with  
    | Ok (hd::tl) when hd = Other ['+'] -> 
        let result = BuildMultExp (Ok acc) []
        AddApp (result, BuildAddExp (Ok tl) [])
    | Ok (hd::tl) -> BuildAddExp (Ok tl) (acc @ [hd])
    | Ok [] -> 
        let result = BuildMultExp (Ok acc) []
        NoSum result
    | Error _ -> failwithf "what?"


