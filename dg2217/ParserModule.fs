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
            |EqualToken
            |HexLit of string
            |NegativeInteger of string
            |AddToken
            |MultToken
            |Other of string
            |SubToken
            |DivToken
            |Unexpected

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
    | Bracket of AST

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

    
let rec makeLeftAppList (inp:AST list) : AST =
// takes an Fsharp list and makes a left associative FuncApp tree representing the items
    match inp with
    | [Bracket el]  ->  el
    | [el] -> el
    | (Bracket hd)::tl -> Funcapp (makeLeftAppList tl, hd)
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
    | PMATCH (OpenRoundBracket) (PBUILDADDEXP(ast, PMATCH (CloseRoundBracket) (inp'))) -> 
                                                                                           let ast' = ast
                                                                                                    |> extractRightAppList []
                                                                                                    |> List.rev
                                                                                                    |> makeLeftAppList
                                                                                                    |> Bracket
                                                                                           Ok(ast', inp') 
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
   printf "entered appexp with %A \n" inp
   match inp with
    | PITEM (Ok(s, lst)) -> 
                            match lst with 
                            |Ok (hd::tl) when hd <> CloseRoundBracket -> 
                                                                let result = buildAppExp (lst) 
                                                                (Funcapp(s, fst(result)), snd(result)) 

                            | _ -> (s, lst)
    | PITEM (Error lst) -> printf "aaaaa \n" ;failwithf "Lst failed %A \n" lst
    | Error msg -> printf "aaaaa \n";failwithf "What? %A" msg
    | Ok _ -> printf "aaaaa \n"; failwithf "What? Can't happen" 
    | _ ->  printf "aaaaa \n";failwithf "What? Can't happen"

and buildMultExp (inp: Result<Token list, Token list>) (acc:Token list):(AST* Result<Token list, Token list>) = 
    match inp with  
    | PMATCH (OpenRoundBracket) (TAKEWHILENOTINBRACKET (inp', acc'))  ->
                                                                        let acc'' = (acc@[OpenRoundBracket] @acc')
                                                                        buildMultExp  inp' (acc'')
    | Ok (hd::tl) when hd = MultToken -> 
        let appResult = buildAppExp (Ok acc)
        let multResult = buildMultExp (Ok tl) []
        (Funcapp(Funcapp(BuiltInFunc (Math Mult), fst(appResult)), fst(multResult)), snd (multResult))
    | Ok (hd::tl) when hd = DivToken -> 
        let appResult = buildAppExp (Ok acc)
        let multResult = buildMultExp (Ok tl) []
        (Funcapp(Funcapp(BuiltInFunc (Math Div), fst(appResult)), fst(multResult)), snd (multResult))
    | Ok (hd::tl) -> buildMultExp (Ok tl) (acc @ [hd])
    | Ok [] -> 
           let res = buildAppExp (Ok acc)
           (fst(res), snd(res))
    | Error _ -> failwithf "what?"

and takeWhileNotInBracket acc inp count = 
    match inp with 
    | Ok (hd::tl) when hd = CloseRoundBracket ->
                                                let count' = count - 1
                                                match count' with 
                                                | 0 -> (Ok (tl),acc@[hd])
                                                | _ -> takeWhileNotInBracket  (acc@[hd]) (Ok tl) count'
    | Ok (hd::tl) when hd = OpenRoundBracket -> takeWhileNotInBracket  (acc@[hd]) (Ok tl) (count+1)
    | Ok (hd::tl) -> takeWhileNotInBracket (acc@[hd]) (Ok tl) count
    | Ok [] -> (Ok [], acc)
    | _ -> failwithf "what?"

and (|TAKEWHILENOTINBRACKET|_|) inp =  Some (takeWhileNotInBracket [] inp 1)

and buildAddExp  (acc:Token list) (inp: Result<Token list, Token list>):(AST* Result<Token list, Token list>) =
    match inp with
    | PMATCH (OpenRoundBracket) (TAKEWHILENOTINBRACKET (inp', acc'))  ->
                                                                        let acc'' = (acc@[OpenRoundBracket] @acc')
                                                                        buildAddExp (acc'') inp'
    | Ok (hd::tl) when hd = AddToken -> 
        let multResult =  buildMultExp (Ok acc) []
        let addResult = buildAddExp [] (Ok tl)
        (Funcapp(Funcapp(BuiltInFunc (Math Add), fst multResult), fst addResult ), snd addResult )
    | Ok (hd::tl) when hd = SubToken -> 
        let multResult =  buildMultExp (Ok acc) []
        let addResult = buildAddExp [] (Ok tl) 
        (Funcapp(Funcapp(BuiltInFunc (Math Sub), fst multResult), fst addResult), snd addResult)
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
                        | (Other x), (EqualToken) ->
                                                        let body = parse (Ok tl)
                                                        Lambda{InputVar=x;Body=body}
                        | (EqualToken), _ -> parse (Ok tl)
                        | _ -> failwithf "Invalid arguments"
    | _ -> failwithf "insufficient expression"

and extractParts inp acc = 
    match inp with 
    | hd::tl when hd = Keyword "in" -> acc,tl
    | hd::tl -> extractParts tl (acc @ [hd])
    | [] -> failwithf "No expression evaluated"
    
and  buildFunctionDef inp  = 
    match inp with 
    | hd::tl  -> match hd with 
                 | Other x -> 
                    let body,expression = extractParts tl []
                    FuncDefExp {Name=x;Body=buildLambda body; Expression=parse(Ok expression)}
                 | _ -> failwithf "Not a valid function name"
    | _ -> failwithf "insufficient expression LET X"

and parse (inp: Result<Token list, Token list>)  = 
    match inp with
    | PMATCH (Let) (Ok rest) -> buildFunctionDef (rest) 
    | _ -> buildAddExp [] inp
            |> fst
            |> extractRightAppList []
            |> List.rev
            |> makeLeftAppList

parse (Ok [Other "f"; OpenRoundBracket; Other "x"; AddToken; IntegerLit 1; CloseRoundBracket])

