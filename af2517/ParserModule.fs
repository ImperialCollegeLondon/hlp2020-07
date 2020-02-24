module ParserModule
type Token = OpenRoundBracket
            |CloseRoundBracket
            |OpenSquareBracket
            |CloseSquareBracket
            |IntegerLit of int
            |StringLit of string
            |SpaceLit
            |DecimalLit of float
            |Keyword of string
            |Let
            |RightArrow
            |Equal
            |HexLit of string
            |NegativeInteger of string
            |Other of string

type MathType = 
   | Add 
   | Sub
   | Div
   | Mult
   | Mod

type BuiltInType = 
    | Math of MathType
    | Equal //works for strings ints and nulls 
    | Explode 
    | Implode 
    | P //creates a pair 
    | PFst 
    | PSnd
    | IsPair
    | IfThenElse  


type AST = 
    | FuncDefExp of FuncDefExpType 
    | Lambda of LambdaType
    | Var of string //only valid in lambdas 
    | Funcapp of AST*AST
    | Pair of AST*AST 
    | Null 
    | Literal of LitType 
    | BuiltInFunc of BuiltInType

and FuncDefExpType = {
    Name: string;
    Body: AST
    Expression: AST
}

and LambdaType = {
    InputVar: string
    Body: AST
}
and LitType = 
    | Int of int 
    | String of string
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
    | Ok []  -> None
    | Ok (s :: rest) when s = tok -> Some(Ok rest)
    | Ok lst -> None
    | Error lst -> None

let builtInFuncMap = ["mod", BuiltInFunc(Math Mod);"equals", BuiltInFunc Equal;"explode", BuiltInFunc Explode;"implode", BuiltInFunc Implode;"pair", BuiltInFunc P;"fst", BuiltInFunc PFst ;"snd", BuiltInFunc PSnd;"ispair", BuiltInFunc IsPair] |> Map.ofList

let rec (|PITEM|_|) (tokLst: Result<Token list, Token list>)  =
    match tokLst with
    | Ok [] -> Error (None)
    | PSITEM (Ok(ast, Ok lst)) ->  Ok (ast, Ok lst)
    | PMATCH (OpenRoundBracket) (PBUILDADDEXP(ast, PMATCH (CloseRoundBracket) (inp'))) ->  Ok(ast, inp') //failwithf "got this ast %A" ast 
    | Ok lst -> Error (Some lst)
    | Error lst -> Error (Some lst)
    |> Some

and (|PSITEM|_|) tokLst = 
    match tokLst with
    | Ok (Other s::rest) when Map.containsKey s builtInFuncMap -> Some(Ok (builtInFuncMap.[s] , Ok rest ))
    | Ok (Other s::rest) when s = "TRUE" ->Some( Ok (Lambda {InputVar="x";Body=Lambda{InputVar="y"; Body=Var"x"} }, Ok rest))
    | Ok (Other s::rest) when s = "FALSE" -> Some(Ok (Lambda {InputVar="x";Body=Lambda{InputVar="y"; Body=Var "y"} }, Ok rest))
    | Ok (Other s :: rest) -> Some( Ok (Var s, Ok rest))
    | Ok (IntegerLit s:: rest) ->Some( Ok (Literal (Int s) ,Ok rest))
    | Ok (StringLit s::rest) ->Some( Ok (Literal (String s), Ok rest))
    | _ -> None

and buildAppExp(inp: Result<Token list, Token list>):(AST* Result<Token list, Token list>) =
    match inp with
    | PITEM (Ok(s, lst)) -> //printf "Tried this: AST: %A and lst: %A \n" s lst
                            match lst with 
                            //| PITEM (Ok(_, _)) -> 
                               // let result = buildAppExp (lst)
                                //failwithf "the result is %A" (fst(result)) //this is triggered first
                                //printf "Matched second PITEM \n"; (Funcapp(s, fst(result)), snd(result))
                            |Ok (hd::tl) when hd <> CloseRoundBracket -> 
                                                                let result = buildAppExp (lst)   
                                                                //(Funcapp(s, fst(result)), snd(result)) 
                                                                let ast = 
                                                                        result
                                                                        |> fst
                                                                        |> extractRightAppList []
                                                                        |> List.rev
                                                                        |> makeLeftAppList
                                                                (Funcapp(s, ast), snd(result)) 

                            //| PITEM (Error (None)) -> printf "Matched empty list \n"  ;(s, lst) 
                            //| PITEM (Error (Some lst')) -> printf "Matched non-empty error list \n" ; (s, lst)
                            | _ -> (s, lst)
    | PITEM (Error lst) -> printf "aaaaa \n" ;failwithf "Lst failed %A " lst
    | Error msg -> printf "aaaaa \n";failwithf "What? %A" msg
    | Ok _ -> printf "aaaaa \n"; failwithf "What? Can't happen" 
    | _ ->  printf "aaaaa \n";failwithf "What? Can't happen"

and buildMultExp (inp: Result<Token list, Token list>) (acc:Token list):(AST* Result<Token list, Token list>) = 
    match inp with  
    | Ok (hd::tl) when hd = Other "*" -> 
        let result = buildAppExp (Ok acc)
                     |> fst
                     |> extractRightAppList []
                     |> List.rev
                     |> makeLeftAppList
        (Funcapp(Funcapp(BuiltInFunc (Math Mult), result), fst(buildMultExp (Ok tl) [])), snd (buildAppExp (Ok acc)))
    | Ok (hd::tl) when hd = Other "/" -> 
        let result = buildAppExp (Ok acc)
                     |> fst
                     |> extractRightAppList []
                     |> List.rev
                     |> makeLeftAppList
        (Funcapp(Funcapp(BuiltInFunc (Math Div), result), fst(buildMultExp (Ok tl) [])), snd (buildAppExp (Ok acc)))
    | Ok (hd::tl) -> buildMultExp (Ok tl) (acc @ [hd])
    | Ok [] -> //problem here 
           let res = buildAppExp (Ok acc)
           (fst(res), snd(res))
    | Error _ -> failwithf "what?"

and buildAddExp  (acc:Token list) (inp: Result<Token list, Token list>):(AST* Result<Token list, Token list>) =
  
    match inp with  
    | Ok (hd::tl) when hd = Other "+" -> 
        let MultResult =  buildMultExp (Ok acc) []
        let AddResult = buildAddExp [] (Ok tl)
        (Funcapp(Funcapp(BuiltInFunc (Math Add), fst MultResult), fst AddResult ), snd MultResult )
    | Ok (hd::tl) when hd = Other "-" -> 
        let MultResult =  buildMultExp (Ok acc) []
        let AddResult = buildAddExp [] (Ok tl) 
        (Funcapp(Funcapp(BuiltInFunc (Math Sub), fst MultResult), fst AddResult), snd MultResult)
    | Ok (hd::tl) -> buildAddExp (acc @ [hd]) (Ok tl) 
    | Ok [] -> 
        buildMultExp (Ok acc) []
    | Error lst -> failwithf "what %A?" lst

and (|PBUILDADDEXP|_|) (inp: Result<Token list, Token list>) = 
    Some (buildAddExp [] inp)

let rec buildLambda inp = 
    match inp with 
    | hd::(hd'::tl) -> match hd,hd' with 
                        | (Other x),(Other y) -> Lambda{InputVar=x;Body=buildLambda(hd'::tl)}
                        | (Other x), (Keyword "=") -> Lambda{InputVar=x;Body=fst(buildAddExp [] (Ok tl))}
                        | (Keyword "="), _ ->  fst(buildAddExp [] (Ok tl)) //let f = 3 for example
                        | _ -> failwithf "Invalid arguments"
    | _ -> failwithf "insufficient expression"

let rec extractParts inp acc = 
    match inp with 
    | hd::tl when hd = Keyword "in" -> acc,tl
    | hd::tl -> extractParts tl (acc @ [hd])
    | [] -> failwithf "No expression evaluated"
    
let rec buildFunctionDef inp  = 
    match inp with 
    | hd::tl  -> match hd with 
                 | Other x -> 
                    let body,expression = extractParts tl []
                    FuncDefExp {Name=x;Body=buildLambda body; Expression=parse(Ok expression)}
                 | _ -> failwithf "Not a valid function name"
    | _ -> failwithf "insufficient expression LET X"

and parse (inp: Result<Token list, Token list>)  = 
    match inp with
    | PMATCH (Keyword "let") (Ok rest) -> buildFunctionDef (rest) 
    | _ -> fst(buildAddExp [] inp)