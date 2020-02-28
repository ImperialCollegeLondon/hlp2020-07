module ParserModule
open Expecto
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
    | Var of char list //only valid in lambdas 
    | FuncApp of AST*AST
    | Pair of AST*AST 
    | Null 
    | Literal of LitType 
    | BFunc of BuiltInType
    | Bracket of AST
    | Y
    | Lazy of AST

and FuncDefExpType = {
    Name: char list;
    Body: AST
    Expression: AST
}

and LambdaType = {
    InputVar: char list
    Body: AST
}
and LitType = 
    | Int of int 
    | String of string


    
let rec extractRightAppList (lst:AST list) (inp:AST) : AST list = 
    match inp with 
    | FuncApp (hd, tl) -> 
        lst @ (extractRightAppList [] hd) @ (extractRightAppList [] tl)
    | el ->  lst @ [el]

    
let rec makeLeftAppList (inp:AST list) : AST =
    match inp with
    | [Bracket el]  ->  el
    | [el] -> el
    | (Bracket hd)::tl -> FuncApp (makeLeftAppList tl, hd)
    | hd::tl -> FuncApp (makeLeftAppList tl, hd)
    | [] -> failwithf "What? Can't happen"

let leftAssociate ast = 
    ast
    |> extractRightAppList []
    |> List.rev
    |> makeLeftAppList
     
let rec takeInsideTokens openingToken closingToken acc inp count = 
    match inp with 
    | Ok (hd::tl) when hd = closingToken ->
                                                let count' = count - 1
                                                match count' with 
                                                | 0 -> Ok(acc,(Ok ([hd]@tl)))
                                                | _ -> takeInsideTokens openingToken closingToken (acc@[hd]) (Ok tl) count'
    | Ok (hd::tl) when hd = openingToken -> takeInsideTokens openingToken closingToken (acc@[hd]) (Ok tl) (count+1)
    | Ok (hd::tl) -> takeInsideTokens openingToken closingToken (acc@[hd]) (Ok tl) count
    | Ok [] -> Error "The tokens don't match"
    | _ -> failwithf "What? Can't happen"

let (|PTAKEINSIDETOKENS|_|) openingToken closingToken inp = Some(takeInsideTokens openingToken closingToken [] inp 1)


let (|PMATCH|_|) (tok: Token) (tokLst: Result<Token list, Token list>) = 
    match tokLst with
    | Ok []  -> None
    | Ok (s :: rest) when s = tok -> Some(Ok rest)
    | Ok lst -> None
    | Error lst -> None






let builtInFuncMap = ["mod", BFunc(Math Mod);"equals", BFunc Equal;"explode", BFunc Explode;"implode", BFunc Implode;"pair", BFunc P;"fst", BFunc PFst ;"snd", BFunc PSnd;"ispair", BFunc IsPair] |> Map.ofList

let rec (|PITEM|_|) (tokLst: Result<Token list, Token list>):(Result<Result<AST,string>*Result<Token list, Token list>,Token list>) option =
    match tokLst with
    | Ok [] -> Error []
    | PNOTEXPITEM (Ok(ast, Ok lst)) ->  Ok (ast, Ok lst)
    | PMATCH (OpenRoundBracket) (PMATCH (Keyword "fun") (BUILDLAMBDA (lamb, PMATCH (CloseRoundBracket) (inp') ) )) ->
        Ok (lamb, inp')
    
    | PMATCH (OpenRoundBracket) (PBUILDADDEXP(Ok ast, PMATCH (CloseRoundBracket) (inp'))) -> 
        let ast' = Bracket(ast)
        Ok(Ok(ast'), inp')

    | PMATCH (Keyword "if") (PTAKEINSIDETOKENS (Keyword "if") (Keyword "then") (Ok (lst, PMATCH (Keyword "then") (PTAKEINSIDETOKENS (Keyword "then") (Keyword "else") (Ok (lst', PMATCH (Keyword "else") (PTAKEINSIDETOKENS (Keyword "else") (Keyword "fi") (Ok (lst'', PMATCH (Keyword "fi") (inp')) ) )) ) )))) ->
         let firstItemParsed = parse (Ok lst)
         let secondItemParsed = parse (Ok lst')
         let thirditemParsed = parse (Ok lst'')
         match firstItemParsed,secondItemParsed,thirditemParsed with 
         | (Ok ast, Ok []),(Ok ast', Ok []),(Ok ast'', Ok []) -> Ok (Ok(Bracket(FuncApp(FuncApp(ast, Lazy (ast')), Lazy (ast'')))), inp')
         | _ -> Error []

    | PMATCH (OpenSquareBracket) (BUILDLIST (ast, (PMATCH (CloseSquareBracket) (inp')))) -> Ok (ast, inp')
    | Ok lst -> Error ( lst)
    | Error lst -> Error (lst)
    |> Some

and (|PNOTEXPITEM|_|) tokLst = 
    match tokLst with
    | Ok (Other s::rest) when Map.containsKey s builtInFuncMap -> Some(Ok (Ok(builtInFuncMap.[s]) , Ok rest ))
    | Ok (Other s::rest) when s = "TRUE" ->Some( Ok (Ok(Lambda {InputVar=Seq.toList "x";Body=Lambda{InputVar=Seq.toList "y"; Body=Var(Seq.toList"x")} }), Ok rest))
    | Ok (Other s::rest) when s = "FALSE" -> Some(Ok (Ok(Lambda {InputVar=Seq.toList "x";Body=Lambda{InputVar=Seq.toList "y"; Body=Var(Seq.toList "y")} }), Ok rest))
    | Ok (Other s :: rest) -> Some( Ok (Ok(Var(Seq.toList s)), Ok rest))
    | Ok (IntegerLit s:: rest) ->Some( Ok (Ok(Literal (Int s)) ,Ok rest))
    | Ok (StringLit s::rest) ->Some( Ok (Ok(Literal (String s)), Ok rest))
    | _ -> None

and endKeyWordsList = [CloseRoundBracket; CloseSquareBracket; Keyword "then"; Keyword "else"; Keyword "fi"; Keyword ";"]

and buildAppExp(inp: Result<Token list, Token list>):(Result<AST,string>*Result<Token list, Token list>) =
   printf "Entered BuildAppExp with %A \n" inp
   match inp with
   | PITEM (Ok(Ok s, lst)) -> 
        match lst with 
        | Ok (hd::tl) when not (List.contains hd endKeyWordsList) -> 
            let result = buildAppExp (lst)
            match result with 
            | (Ok ast, rest) -> 
                (Ok(FuncApp(s, ast)), rest) 
            | (Error msg, rest) -> (Error msg, rest)
        | _ -> (Ok s, lst)
    
   | PITEM (Error lst) -> (Error "Couldn't parse item", Error lst)
   | lst -> (Error "Input is not vaid", lst)




and buildMultExp (acc:Token list) (inp: Result<Token list, Token list>) :(Result<AST,string>*Result<Token list, Token list>) = 
    printf "Entered BuildMultExp with %A \n" inp
    match inp with  
    | PMATCH (OpenRoundBracket) (PTAKEINSIDETOKENS (OpenRoundBracket) (CloseRoundBracket) (Ok(acc', inp')))  ->
        let acc'' = (acc@[OpenRoundBracket] @acc')
        buildMultExp (acc'') inp' 
    
    | PMATCH (OpenSquareBracket) (PTAKEINSIDETOKENS (OpenSquareBracket) (CloseSquareBracket) (Ok(acc', inp')))  ->
        let acc'' = (acc@[OpenSquareBracket] @acc')
        buildMultExp (acc'') inp' 


    | PMATCH (Keyword "if") (PTAKEINSIDETOKENS (Keyword "if") (Keyword "fi") (Ok(acc', inp'))) ->
        let acc'' = (acc@[Keyword "if"] @acc')
        buildMultExp   (acc'') inp'

    | Ok (hd::tl) when hd = MultToken -> 
        let appResult = buildAppExp (Ok acc)
        let multResult = buildMultExp  [] (Ok tl)
        match appResult,multResult with 
        | ((Ok appAST, _),(Ok multAST, rest)) -> 
            (Ok (FuncApp(FuncApp(BFunc (Math Mult), leftAssociate appAST), multAST )), rest )

        | (Error msg, rest),_ -> (Error msg, rest)
        | _,(Error msg, rest) -> (Error msg, rest)
    
    | Ok (hd::tl) when hd = DivToken -> 
        let appResult = buildAppExp (Ok acc)
        let multResult = buildMultExp  [] (Ok tl)
        match appResult,multResult with 
        | ((Ok appAST, _),(Ok multAST, rest)) -> 
             let appAST' = leftAssociate appAST
             (Ok (FuncApp(FuncApp(BFunc (Math Div), appAST'), multAST )), rest )
        | (Error msg, rest),_ -> (Error msg, rest)
        | _,(Error msg, rest) -> (Error msg, rest)
    
    | Ok (hd::tl) -> buildMultExp  (acc @ [hd]) (Ok tl)
    | Ok [] -> 
         let res = buildAppExp (Ok acc)
         match res with 
         | (Ok appAST, rest) -> 
             let appAST' = leftAssociate appAST
             (Ok appAST', rest )
         | (Error msg, rest) -> (Error msg, rest)
   
    | Error _ -> failwithf "what?"



and buildAddExp  (acc:Token list) (inp: Result<Token list, Token list>):(Result<AST,string>*Result<Token list, Token list>) =
    printf "Entered BuildAddExp with %A \n" inp
    match inp with
    | PMATCH (OpenRoundBracket) (PTAKEINSIDETOKENS (OpenRoundBracket) (CloseRoundBracket) (Ok(acc', inp')))  ->
        let acc'' = (acc@[OpenRoundBracket] @acc')
        buildAddExp (acc'') inp'

    | PMATCH (OpenSquareBracket) (PTAKEINSIDETOKENS (OpenSquareBracket) (CloseSquareBracket) (Ok(acc', inp')))  ->
        let acc'' = (acc@[OpenSquareBracket] @acc')
        buildAddExp (acc'') inp' 
    
    //| PMATCH (Keyword "if") (TAKEINSIDEIF (Ok(inp', acc'))) ->
    //    let acc'' = (acc@[Keyword "if"] @acc')
    //    buildAddExp (acc'') inp'

    | PMATCH (Keyword "if") (PTAKEINSIDETOKENS (Keyword "if") (Keyword "fi") (Ok(acc', inp'))) ->
        let acc'' = (acc@[Keyword "if"] @acc')
        buildAddExp (acc'') inp'
    
    | Ok (hd::tl) when hd = AddToken -> 
        let multResult =  buildMultExp [] (Ok acc) 
        let addResult = buildAddExp [] (Ok tl)
        match multResult,addResult with 
        | ((Ok multAST, _),(Ok addAST, rest)) -> (Ok (FuncApp(FuncApp(BFunc (Math Add), multAST), addAST )), rest )
        | (Error msg, rest),_ -> (Error msg, rest)
        | _,(Error msg, rest) -> (Error msg, rest)
    
    | Ok (hd::tl) when hd = SubToken -> 
        let multResult =  buildMultExp [] (Ok acc) 
        let addResult = buildAddExp [] (Ok tl)
        match multResult,addResult with 
        | ((Ok multAST, _),(Ok addAST, rest)) -> (Ok (FuncApp(FuncApp(BFunc (Math Sub), multAST), addAST )), rest )
        | (Error msg, rest),_ -> (Error msg, rest)
        | _,(Error msg, rest) -> (Error msg, rest)
    
    | Ok (hd::tl) -> buildAddExp (acc @ [hd]) (Ok tl) 
    | Ok [] -> buildMultExp [] (Ok acc)  
    | Error lst -> (Error "Invalid input expression" , Error lst)

and (|PBUILDADDEXP|_|) (inp: Result<Token list, Token list>):(Result<AST,string>*Result<Token list, Token list>)option = 
    Some (buildAddExp [] inp)

and buildLambda (inp:Result<Token list, Token list>):(Result<AST,string>*Result<Token list, Token list>) = 
    match inp with 
    | Ok (hd::(hd'::tl)) -> 
        match hd,hd' with 
        | (Other x),(Other y) ->
            let result = buildLambda(Ok (hd'::tl))
            match result with 
            | (Ok ast, rest) -> (Ok(Lambda{InputVar=Seq.toList x;Body=ast}),rest)
            | (Error msg, rest) -> (Error msg, rest)

        | (Other x), (EqualToken) ->
            let body = parse (Ok tl)
            match body with 
            | (Ok ast, rest) -> (Ok(Lambda{InputVar=Seq.toList x;Body=ast}), rest)
            | (Error msg, rest) -> (Error msg, rest)
        | (EqualToken), _ -> parse (Ok (hd'::tl))
        
        | _ -> (Error "Invalid arguments" , inp)
    | _ -> (Error "insufficient expression" , inp )

and (|BUILDLAMBDA|_|) inp:((Result<AST,string>*Result<Token list, Token list>) option) = Some (buildLambda inp)

and extractParts inp acc = 
    match inp with 
    | hd::tl when hd = Other "in" -> Ok( acc,tl)
    | hd::tl -> extractParts tl (acc @ [hd])
    | [] -> Error "This function definition is never used"
    
and  buildFunctionDef inp:(Result<AST,string>*Result<Token list, Token list>)  = 
    match inp with 
    | hd::tl  -> 
        match hd with 
            | Other x -> 
                let splitFunc = extractParts tl []
                match splitFunc with 
                | Ok (body, expression) ->
                    let parsedExpression = parse (Ok expression)
                    let parsedBody = buildLambda (Ok body)
                    match  parsedBody, parsedExpression with 
                    |((Ok body, _),(Ok expression, rest))  -> (Ok (FuncDefExp {Name=Seq.toList x;Body=body; Expression=expression}),rest)
                    | ((Error msg, rest), _) -> (Error msg, rest)
                    | (_, (Error msg, rest)) -> (Error msg, rest)

                | Error msg -> (Error msg, Error inp)
            
            | _ -> (Error  "No name found for this function definition", Error inp)

    | _ -> (Error "Insufficient elements in function definition" , Error inp )

and parse (inp: Result<Token list, Token list>):(Result<AST,string>*Result<Token list, Token list>)  = 
    match inp with
    | PMATCH (Let) (Ok rest) -> 
        let result = buildFunctionDef (rest)
        match result with 
        | (Ok _, Ok []) -> result
        | (Ok _, rest) -> (Error "Ilegal expression at the end", rest )
        | (Error msg, rest) -> (Error msg,  rest)
    | _ -> buildAddExp [] inp


and buildList inp :(Result<AST,string>*Result<Token list, Token list>) = 
    match inp with
    | PBUILDADDEXP (Ok ast, (PMATCH (Keyword ";") (inp'))) -> 
        let result = buildList inp'
        match result with 
        | (Ok ast', rest) ->  (Ok(Pair (ast, ast')), rest)
        | (Error msg, rest) -> (Error msg, rest)
    | PBUILDADDEXP (Ok ast, rest) -> (Ok ast, rest)
    | PBUILDADDEXP (Error msg, rest) -> (Error msg, rest)
    | _ -> failwithf "What? Can't happen PBUILDADDEXP always matches"

and (|BUILDLIST|_|) inp:(Result<AST,string>*Result<Token list, Token list>) option = Some(buildList inp)

let makeTests (name, instr, outI) =
    test name {
        let tokLst = instr
        match (tokLst |> parse |> fst) with 
        | Ok ast -> Expect.equal (Ok ast) outI (sprintf "%A" tokLst)
        | Error msg -> Expect.equal (Error msg) outI (sprintf "%A" tokLst)
    }
let testListWithExpecto = 
    [
        "first test",(Ok [Other "x"; Other "y"]),(Ok(FuncApp(Var ['x'], Var ['y'])))
        "second test",(Ok [Keyword "if"; Other "x";  IntegerLit 1; Keyword "then"; Other "y"; IntegerLit 1; Keyword "else"; Other "z"; IntegerLit 1; Keyword "fi"]) ,(Ok(FuncApp(FuncApp(FuncApp (Var ['x'],Literal (Int 1)),Lazy (FuncApp (Var ['y'],Literal (Int 1)))),Lazy (FuncApp (Var ['z'],Literal (Int 1))))))
    ]
    |> List.map makeTests
    |> testList "Set of tests"

let testsWithExpecto() =
    runTests defaultConfig testListWithExpecto |> ignore
