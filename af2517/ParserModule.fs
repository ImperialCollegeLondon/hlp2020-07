module ParserModule

let test() = printfn "sample top level function in ParserModule" 
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
    | True of AST //make it a named function with Lambda ("A",Lambda ("B",Var "A" ))
    | False of AST 

 //1+2 -> AddMult(JustAppExp(AppExpItem(Literal(Int 1))), JustMultExp(JustAppExp(AppExpItem(Literal(Int 2)))))
type AppExp = AppExpItem of AST | AppExpExp of AST*AppExp
type MultExp = JustAppExp of AppExp | MultApp of AppExp*MultExp
type AddExp = JustMultExp of MultExp | AddMult of MultExp*AddExp  
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
//one recursive function needed for every precedence level 
//[Other "f"; Other "x"] -> AppExpExp(f,AppExpItem(x))
//<applicative-exp> ::= <item> | <item> <applicative-exp>
let rec PappExp(inp: Result<Token list, Token list>):(AppExp* Result<Token list, Token list>) =
    match inp with
    // TWO CASES IF PAPP MATCHES AGAIN THEN APPEXPEXP ELSE APPEXPITEM
    | PITEM (Ok(s, lst)) -> match Ok lst with 
                                | PITEM (Ok(s', lst')) ->  (AppExpExp(s, fst(PappExp (Ok lst))), snd(PappExp (Ok lst)))
                                | _ -> ((AppExpItem s), Ok lst) 
    | Error msg -> failwithf "What? Can't happen%A" msg
    | Ok lst ->  failwithf "What? Can't happen%A" lst
    | _ ->  failwithf "What? Can't happen"


