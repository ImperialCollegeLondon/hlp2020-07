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


    
let rec extractRightAppList (lst:AST list) (inp:AST) : AST list = 
    match inp with 
    | Funcapp (hd, tl) -> lst @ (extractRightAppList [] hd) @ (extractRightAppList [] tl)
    | el ->  lst @ [el]

    
let rec makeLeftAppList (inp:AST list) : AST =
    match inp with
    | [Bracket el]  ->  el
    | [el] -> el
    | (Bracket hd)::tl -> Funcapp (makeLeftAppList tl, hd)
    | hd::tl -> Funcapp (makeLeftAppList tl, hd)
    | [] -> failwithf "What? Can't happen"

let leftAssociate ast = 
    ast
    |> extractRightAppList []
    |> List.rev
    |> makeLeftAppList
     
let rec takeInsideBracket acc inp count = 
    match inp with 
    | Ok (hd::tl) when hd = CloseRoundBracket ->
                                                let count' = count - 1
                                                match count' with 
                                                | 0 -> (Ok (tl),acc@[hd])
                                                | _ -> takeInsideBracket  (acc@[hd]) (Ok tl) count'
    | Ok (hd::tl) when hd = OpenRoundBracket -> takeInsideBracket  (acc@[hd]) (Ok tl) (count+1)
    | Ok (hd::tl) -> takeInsideBracket (acc@[hd]) (Ok tl) count
    | Ok [] -> failwithf "Brackets don't match"
    | _ -> failwithf "what?"

let (|TAKEINSIDEBRACKET|_|) inp =  Some (takeInsideBracket [] inp 1)

let rec takeInsideIf acc inp count = 
    match inp with 
    | Ok (hd::tl) when hd = Keyword "fi" ->
        let count' = count - 1
        match count' with 
        | 0 -> (Ok (tl), acc@[hd])
        | _ -> takeInsideIf (acc@[hd]) (Ok tl) count'
    | Ok (hd::tl) when hd = Keyword "if" -> takeInsideIf (acc@[hd]) (Ok tl) (count + 1)
    | Ok (hd::tl) -> takeInsideIf (acc@[hd]) (Ok tl) count
    | Ok [] -> failwithf "invalid if statement"
    | _ -> failwithf "what?"

let (|TAKEINSIDEIF|_|) inp = Some (takeInsideIf [] inp 1)

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
    | PNOTEXPITEM (Ok(ast, Ok lst)) ->  Ok (ast, Ok lst)
    | PMATCH (OpenRoundBracket) (PMATCH (Keyword "fun") (BUILDLAMBDA (lamb, PMATCH (CloseRoundBracket) (inp') ) )) ->
        Ok (lamb, inp')
    
    | PMATCH (OpenRoundBracket) (PBUILDADDEXP(ast, PMATCH (CloseRoundBracket) (inp'))) -> 
        let ast' = leftAssociate ast
                   |> Bracket
        Ok(ast', inp')

    | PMATCH (Keyword "if") (PBUILDADDEXP (ast, (PMATCH (Keyword "then") (PBUILDADDEXP (ast', (PMATCH (Keyword "else") (PBUILDADDEXP (ast'', (PMATCH (Keyword "fi") (inp')))))))))) ->
         Ok (Bracket(Funcapp(Funcapp(ast, ast'), ast'')), inp')

    | PMATCH (OpenSquareBracket) (BUILDLIST (ast, (PMATCH (CloseSquareBracket) (inp')))) -> Ok (ast, inp')
    | Ok lst -> Error (Some lst)
    | Error lst -> Error (Some lst)
    |> Some

and (|PNOTEXPITEM|_|) tokLst = 
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

and buildMultExp (acc:Token list) (inp: Result<Token list, Token list>) :(AST* Result<Token list, Token list>) = 
    match inp with  
    | PMATCH (OpenRoundBracket) (TAKEINSIDEBRACKET (inp', acc'))  ->
        let acc'' = (acc@[OpenRoundBracket] @acc')
        buildMultExp (acc'') inp' 
    
    | PMATCH (Keyword "if") (TAKEINSIDEIF (inp', acc')) ->
        let acc'' = (acc@[Keyword "if"] @acc')
        buildMultExp   (acc'') inp'

    | Ok (hd::tl) when hd = MultToken -> 
        let appResult = buildAppExp (Ok acc)
        let multResult = buildMultExp  [] (Ok tl)
        (Funcapp(Funcapp(BuiltInFunc (Math Mult), fst(appResult)), fst(multResult)), snd (multResult))
    
    | Ok (hd::tl) when hd = DivToken -> 
        let appResult = buildAppExp (Ok acc)
        let multResult = buildMultExp  [] (Ok tl)
        (Funcapp(Funcapp(BuiltInFunc (Math Div), fst(appResult)), fst(multResult)), snd (multResult))
    
    | Ok (hd::tl) -> buildMultExp  (acc @ [hd]) (Ok tl)
    | Ok [] -> 
           let res = buildAppExp (Ok acc)
           (fst(res), snd(res))
    
    | Error _ -> failwithf "what?"



and buildAddExp  (acc:Token list) (inp: Result<Token list, Token list>):(AST* Result<Token list, Token list>) =
    match inp with
    | PMATCH (OpenRoundBracket) (TAKEINSIDEBRACKET (inp', acc'))  ->
        let acc'' = (acc@[OpenRoundBracket] @acc')
        buildAddExp (acc'') inp'
    
    | PMATCH (Keyword "if") (TAKEINSIDEIF (inp', acc')) ->
        let acc'' = (acc@[Keyword "if"] @acc')
        buildAddExp (acc'') inp'
    
    | Ok (hd::tl) when hd = AddToken -> 
        let multResult =  buildMultExp [] (Ok acc) 
        let addResult = buildAddExp [] (Ok tl)
        (Funcapp(Funcapp(BuiltInFunc (Math Add), fst multResult), fst addResult ), snd addResult )
    
    | Ok (hd::tl) when hd = SubToken -> 
        let multResult =  buildMultExp [] (Ok acc)
        let addResult = buildAddExp [] (Ok tl) 
        (Funcapp(Funcapp(BuiltInFunc (Math Sub), fst multResult), fst addResult), snd addResult)
    
    | Ok (hd::tl) -> buildAddExp (acc @ [hd]) (Ok tl) 
    | Ok [] -> buildMultExp [] (Ok acc)  
    | Error lst -> failwithf "what %A?" lst

and (|PBUILDADDEXP|_|) (inp: Result<Token list, Token list>) = 
    Some (buildAddExp [] inp)

and buildLambda (inp:Result<Token list, Token list>):(AST*Result<Token list, Token list>) = 
    match inp with 
    | Ok (hd::(hd'::tl)) -> 
        match hd,hd' with 
        | (Other x),(Other y) ->
            let result = buildLambda( Ok (hd'::tl))
            (Lambda{InputVar=x;Body=fst(result)},snd(result))

        | (Other x), (EqualToken) ->
            let body = parse (Ok tl)
            (Lambda{InputVar=x;Body=fst(body)}, snd(body))

        | (EqualToken), _ -> parse (Ok (hd'::tl))
        | _ -> failwithf "Invalid arguments"
    | _ -> failwithf "insufficient expression"

and (|BUILDLAMBDA|_|) inp = Some (buildLambda inp)

and extractParts inp acc = 
    match inp with 
    | hd::tl when hd = Keyword "in" -> acc,tl
    | hd::tl -> extractParts tl (acc @ [hd])
    | [] -> failwithf "No expression evaluated"
    
and  buildFunctionDef inp  = 
    match inp with 
    | hd::tl  -> 
        match hd with 
            | Other x -> 
                let body,expression = extractParts tl []
                printf "Body is %A \n" body
                printf "Expression is %A \n" expression
                let expression = parse(Ok expression)
                printf "Parsed expression is %A \n" expression
                (FuncDefExp {Name=x;Body=fst(buildLambda (Ok body)); Expression=fst(expression)},snd(expression))
            
            | _ -> failwithf "Not a valid function name"

    | _ -> failwithf "insufficient expression LET X"

and parse (inp: Result<Token list, Token list>):(AST*Result<Token list, Token list>)  = 
    match inp with
    | PMATCH (Let) (Ok rest) -> buildFunctionDef (rest) 
    | _ -> 
          let res = buildAddExp [] inp
          let ast = res
                    |> fst
                    |>leftAssociate
          (ast, snd(res))

and buildList inp = 
    match inp with
    | PBUILDADDEXP (ast, (PMATCH (Keyword ";") (inp'))) -> 
        let result = buildList inp'
        (Pair (ast, fst(result)), snd(result))
    | PBUILDADDEXP (ast, Ok inp') -> (ast, Ok inp')
    | _ -> failwithf "Invalid input"

and (|BUILDLIST|_|) inp = Some(buildList inp)