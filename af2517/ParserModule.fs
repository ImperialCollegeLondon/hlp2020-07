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

    
let rec extractRightAppList (lst:AST list) (inp:AST) : AST list = 
// extracts the top-level right-associative application list and returns it as an Fsharp list
    match inp with 
    | Funcapp (hd, tl) -> lst @ [hd] @ (extractRightAppList lst tl)
    | el ->  lst @ [el]
    | _ -> failwithf "What? Shouldn't happen"
    
let rec makeLeftAppList (inp:AST list) : AST =
// takes an Fsharp list and makes a left associative FuncApp tree representing the items
    match inp with 
    | [el] ->  el
    | hd::tl -> Funcapp (makeLeftAppList tl, hd)
    | [] -> failwithf "What? Can't happen"
     
let (|PMATCH|_|) (tok: Token) (tokLst: Result<Token list, Token list>) = 
    match tokLst with
    | _ when tok = Bracket [')'] -> None
    | Ok []  -> None
    | Ok (s :: rest) when s = tok -> Some(Ok rest)
    | Ok lst -> None
    | Error lst -> None
    
let builtInFuncMap = [['m';'o';'d'], BuiltInFunc Mod;['e';'q';'u';'a';'l';'s'], BuiltInFunc Equal;['e';'x';'p';'l';'o';'d';'e'], BuiltInFunc Explode;['i';'m';'p';'l';'o';'d';'e'], BuiltInFunc Implode;['p';'a';'i';'r'], BuiltInFunc P;['f';'s';'t'], BuiltInFunc PFst ;['s';'n';'d'], BuiltInFunc PSnd;['i';'s';'p';'a';'i';'r'], BuiltInFunc IsPair] |> Map.ofList

let rec (|PITEM|_|) (tokLst: Result<Token list, Token list>)  =
    match tokLst with
    | Ok [] -> Error []
    | Ok (Other s::rest) when Map.containsKey s builtInFuncMap -> Ok (builtInFuncMap.[s] , Ok rest )
    | Ok (Other s :: rest) ->  Ok (Var s, Ok rest)
    | Ok (IntToken s:: rest) -> Ok (Literal (Int s) ,Ok rest)
    | Ok (StringToken s::rest) -> Ok (Literal (String s), Ok rest)
    //| Ok (Bracket s::_) when s = [')'] -> Error []
    | PMATCH (Bracket ['(']) (PBUILDADDEXP(ast, PMATCH (Bracket [')']) (inp'))) -> Ok (ast, inp') 
    | Ok lst -> Error lst
    | Error lst ->  Error lst
    |> Some

and buildAppExp(inp: Result<Token list, Token list>):(AST* Result<Token list, Token list>) =
    match inp with
    | PITEM (Ok(s, lst)) -> match lst with 
                            | PITEM (Ok(_, _)) ->  
                                let result = buildAppExp (lst)
                                (Funcapp(s, fst(result)), snd(result))
                            | _ -> (s, lst)                            
    | PITEM (Error lst) -> failwithf "Lst failed %A " lst
    | Error msg -> failwithf "What? %A" msg
    | Ok _ ->  failwithf "What? Can't happen" 
    | _ ->  failwithf "What? Can't happen"

and buildMultExp (inp: Result<Token list, Token list>) (acc:Token list):(AST* Result<Token list, Token list>) = 
    match inp with  
    | Ok (hd::tl) when hd = Other ['*'] -> 
        let result = buildAppExp (Ok acc)
                     |> fst
                     |> extractRightAppList []
                     |> List.rev
                     |> makeLeftAppList
        (Funcapp(Funcapp(BuiltInFunc Mult, result), fst(buildMultExp (Ok tl) [])), snd (buildAppExp (Ok acc)))
    | Ok (hd::tl) when hd = Other ['/'] -> 
        let result = buildAppExp (Ok acc)
                     |> fst
                     |> extractRightAppList []
                     |> List.rev
                     |> makeLeftAppList
        (Funcapp(Funcapp(BuiltInFunc Div, result), fst(buildMultExp (Ok tl) [])), snd (buildAppExp (Ok acc)))
    | Ok (hd::tl) -> buildMultExp (Ok tl) (acc @ [hd])
    | Ok [] -> 
           let res = buildAppExp (Ok acc)
                     |> fst
                     |> extractRightAppList []
                     |> List.rev
                     |> makeLeftAppList
           (res, Ok [])
    | Error _ -> failwithf "what?"

and buildAddExp  (acc:Token list) (inp: Result<Token list, Token list>):(AST* Result<Token list, Token list>) = 
    match inp with  
    | Ok (hd::tl) when hd = Other ['+'] -> 
        let MultResult =  buildMultExp (Ok acc) []
        let AddResult = buildAddExp [] (Ok tl)
        (Funcapp(Funcapp(BuiltInFunc Add, fst MultResult), fst AddResult ), snd MultResult )
    | Ok (hd::tl) when hd = Other ['-'] -> 
        let MultResult =  buildMultExp (Ok acc) []
        let AddResult = buildAddExp [] (Ok tl) 
        (Funcapp(Funcapp(BuiltInFunc Sub, fst MultResult), fst AddResult), snd MultResult)
    | Ok (hd::tl) -> buildAddExp (acc @ [hd]) (Ok tl) 
    | Ok [] -> 
        buildMultExp (Ok acc) []
    | Error lst -> failwithf "what %A?" lst

and (|PBUILDADDEXP|_|) (inp: Result<Token list, Token list>) = 
    Some (buildAddExp [] inp)

let rec buildLambda inp = 
    match inp with 
    | hd::(hd'::tl) -> match hd,hd' with 
                        | (Other x),(Other y) -> Lambda(x, buildLambda (hd'::tl))
                        | (Other x), (Keyword EQUAL) -> Lambda(x, fst(buildAddExp [] (Ok tl)))
                        | (Keyword EQUAL), _ ->  fst(buildAddExp [] (Ok tl)) //let f = 3 for example
                        | _ -> failwithf "Invalid arguments"
    | _ -> failwithf "insufficient expression"

let rec extractParts inp acc = 
    match inp with 
    | hd::tl when hd = Keyword IN -> acc,tl
    | hd::tl -> extractParts tl (acc @ [hd])
    | [] -> failwithf "No expression evaluated"
    
let rec buildFunctionDef inp  = 
    match inp with 
    | hd::tl  -> match hd with 
                 | Other x -> 
                    let body,expression = extractParts tl []
                    FuncDefExp ((x, buildLambda body), Parse (Ok expression))
                 | _ -> failwithf "Not a valid function name"
    | _ -> failwithf "insufficient expression LET X"

and Parse (inp: Result<Token list, Token list>)  = 
    match inp with
    | PMATCH (Keyword LET) (Ok rest) -> buildFunctionDef (rest) 
    | _ -> fst(buildAddExp [] inp)
