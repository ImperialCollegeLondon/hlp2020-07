module ParserModule
open Definitions


//    print <| takeInsideTokens (Keyword "match") (Keyword "endmatch") [] (Ok <| tokenize "f x y + 1 endmatch case endmatch") 1



let rec takeInsideTokens (openingToken:Token) (closingToken:Token) (acc:Token list) (inp:Result<Token list,Token list>) count : (Result<Token list * Result<Token list, Token list>,string>) = 
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

let split (at:Token) (x:Token list)  : (Token list list * Token list) =
    let rec splitHelper (at:Token) (x:Token list) (acc:Token list) (finalAcc:Token list list) : (Token list list * Token list) =
        match x with
        | [] ->  (finalAcc , acc)
        | hd::tl when hd = Keyword "match"  ->
            match takeInsideTokens (Keyword "match") (Keyword "endmatch") [] (Ok tl) 1 with
            | Ok (mstruct,Ok (_::rest) ) ->
                splitHelper at rest ([hd] @ acc @ mstruct @ [Keyword "endmatch"] ) finalAcc
            | _ -> failwithf "Error while building nested stucture"
        | hd::tl when hd <> at -> splitHelper at tl (acc @ [hd]) finalAcc
        | hd::tl when hd = at ->
            //must be the first time we actually met a split character so we can just split
            splitHelper at tl [] (finalAcc @ [acc])
         | _ -> failwithf "Does this actually happen"
    
    splitHelper at x [] [] 
    //fst res,snd res



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
    | MatchDef of MatchDefType
    | Lambda of LambdaType
    | Var of char list //only valid in lambdas 
    | Funcapp of AST*AST
    | Pair of AST*AST 
    | Null
    | Match of (AST list)
    | Literal of LitType 
    | BuiltInFunc of BuiltInType
    | Bracket of AST
    | Y
    | Lazy of AST

and FuncDefExpType = {
    Name: char list;
    Body: AST
    Expression: AST
}

and MatchDefType = {
    Condition: AST
    Cases: AST list
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
    | Funcapp (hd, tl) -> 
        lst @ (extractRightAppList [] hd) @ (extractRightAppList [] tl)
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
                                                | 0 -> Ok((Ok (tl),acc@[hd]))
                                                | _ -> takeInsideBracket  (acc@[hd]) (Ok tl) count'
    | Ok (hd::tl) when hd = OpenRoundBracket -> takeInsideBracket  (acc@[hd]) (Ok tl) (count+1)
    | Ok (hd::tl) -> takeInsideBracket (acc@[hd]) (Ok tl) count
    | Ok [] -> Error "Brackets don't match"
    | _ -> failwithf "What? Can't happen"

let (|TAKEINSIDEBRACKET|_|) inp =  Some (takeInsideBracket [] inp 1)


let rec takeInsideSqBracket acc inp count = 
    match inp with 
    | Ok (hd::tl) when hd = CloseSquareBracket ->
                                                let count' = count - 1
                                                match count' with 
                                                | 0 -> Ok((Ok (tl),acc@[hd]))
                                                | _ -> takeInsideSqBracket  (acc@[hd]) (Ok tl) count'
    | Ok (hd::tl) when hd = OpenSquareBracket -> takeInsideSqBracket  (acc@[hd]) (Ok tl) (count+1)
    | Ok (hd::tl) -> takeInsideSqBracket (acc@[hd]) (Ok tl) count
    | Ok [] -> Error "Brackets don't match"
    | _ -> failwithf "What? Can't happen"

let (|TAKEINSIDESQBRACKET|_|) inp =  Some (takeInsideSqBracket [] inp 1)

let rec takeInsideIf acc inp count = 
    match inp with 
    | Ok (hd::tl) when hd = Keyword "fi" ->
        let count' = count - 1
        match count' with 
        | 0 -> (Ok(Ok (tl), acc@[hd]))
        | _ -> takeInsideIf (acc@[hd]) (Ok tl) count'
    | Ok (hd::tl) when hd = Keyword "if" -> takeInsideIf (acc@[hd]) (Ok tl) (count + 1)
    | Ok (hd::tl) -> takeInsideIf (acc@[hd]) (Ok tl) count
    | Ok [] -> Error "invalid if statement"
    | _ -> failwithf "what?"


let rec takeInsideMatch acc inp count : Result<(Result<Token list,'b> * Token list),string> =
    match inp with
    //assume this one doesn't see anything
    | Ok (hd::tl) when hd = Keyword "endmatch" ->
        let count' = count - 1
        match count' with
        | 0 ->  (Ok(Ok (tl), acc@[hd]))
        | _ -> takeInsideMatch (acc@[hd]) (Ok tl) count'
    | Ok (hd::tl) when hd = Keyword "match" -> takeInsideMatch (acc@[hd]) (Ok tl) (count + 1)
    | Ok (hd::tl) -> takeInsideMatch (acc@[hd]) (Ok tl) count
    | Ok [] -> Error "invalid match statement"
    | _ -> failwithf "What?"
    
let (|TAKEINSIDEIF|_|) inp = Some (takeInsideIf [] inp 1)

let (|TAKEINSIDEMATCH|_|) inp = Some(takeInsideMatch [] inp 1)

let (|PMATCH|_|) (tok: Token) (tokLst: Result<Token list, Token list>) = 
    match tokLst with
    | Ok []  -> None
    | Ok (s :: rest) when s = tok -> Some(Ok rest)
    | Ok lst -> None
    | Error lst -> None

let builtInFuncMap = ["mod", BuiltInFunc(Math Mod);"equals", BuiltInFunc Equal;"explode", BuiltInFunc Explode;"implode", BuiltInFunc Implode;"pair", BuiltInFunc P;"fst", BuiltInFunc PFst ;"snd", BuiltInFunc PSnd;"ispair", BuiltInFunc IsPair] |> Map.ofList

let rec (|PITEM|_|) (tokLst: Result<Token list, Token list>):(Result<Result<AST,string>*Result<Token list, Token list>,Token list>) option =
    match tokLst with
    | Ok [] -> Error []
    | PNOTEXPITEM (Ok(ast, Ok lst)) ->  Ok (ast, Ok lst)
    | PMATCH (OpenRoundBracket) (PMATCH (Keyword "fun") (BUILDLAMBDA (lamb, PMATCH (CloseRoundBracket) (inp') ) )) ->
        Ok (lamb, inp')
    
    | PMATCH (OpenRoundBracket) (PBUILDADDEXP(Ok ast, PMATCH (CloseRoundBracket) (inp'))) -> 
        let ast' = ast//leftAssociate ast
                   |> Bracket
        Ok(Ok(ast'), inp')

    | PMATCH (Keyword "if") (PBUILDADDEXP (Ok ast, (PMATCH (Keyword "then") (PBUILDADDEXP (Ok ast', (PMATCH (Keyword "else") (PBUILDADDEXP (Ok ast'', (PMATCH (Keyword "fi") (inp')))))))))) ->
         Ok (Ok(Bracket(Funcapp(Funcapp(ast, Lazy ast'), Lazy ast''))), inp')

    | PMATCH (Keyword "match") (PBUILDMATCHCASES(astList, PMATCH (Keyword "endmatch") (inp')))  ->
        //print ast
        
        let res = List.map (function |Ok x -> x |y -> failwithf "One of the cases failed") astList
        //Assumes at least one
        //Not dealt with errors yet
        Ok(Ok (MatchDef{Condition = res.Head; Cases = res.Tail}),inp')
    //ADD HERE
    
    
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

and endKeyWordsList = [CloseRoundBracket; CloseSquareBracket; Keyword "then"; Keyword "else"; Keyword "fi"; Keyword ";"; Keyword "endmatch"; Keyword "case"]

and buildAppExp(inp: Result<Token list, Token list>):(Result<AST,string>*Result<Token list, Token list>) =
   //print "B"
   //print inp
   match inp with
   | PITEM (Ok(Ok s, lst)) -> 
        match lst with 
        | Ok (hd::tl) when not (List.contains hd endKeyWordsList) -> 
            let result = buildAppExp (lst)
            match result with 
            | (Ok ast, rest) -> 
                (Ok(Funcapp(s, ast)), rest) 
            | (Error msg, rest) -> (Error msg, rest)
        | _ -> (Ok s, lst)
    
   | PITEM (Error lst) -> (Error "Couldn't parse item", Error lst)
   | lst -> (Error "Input is not vaid", lst)




and buildMultExp (acc:Token list) (inp: Result<Token list, Token list>) :(Result<AST,string>*Result<Token list, Token list>) = 
    match inp with  
    | PMATCH (OpenRoundBracket) (TAKEINSIDEBRACKET (Ok(inp', acc')))  ->
        let acc'' = (acc@[OpenRoundBracket] @acc')
        buildMultExp (acc'') inp' 
    
    | PMATCH (OpenSquareBracket) (TAKEINSIDESQBRACKET (Ok(inp', acc')))  ->
        let acc'' = (acc@[OpenSquareBracket] @acc')
        buildMultExp (acc'') inp'
        
    | PMATCH (Keyword "match") (TAKEINSIDEMATCH (Ok(inp', acc'))) ->
        let acc'' = (acc@[Keyword "match"] @ acc')
        buildMultExp (acc'') inp'


    | PMATCH (Keyword "if") (TAKEINSIDEIF (Ok(inp', acc'))) ->
        let acc'' = (acc@[Keyword "if"] @acc')
        buildMultExp   (acc'') inp'

    | Ok (hd::tl) when hd = MultToken -> 
        let appResult = buildAppExp (Ok acc)
        let multResult = buildMultExp  [] (Ok tl)
        match appResult,multResult with 
        | ((Ok appAST, _),(Ok multAST, rest)) -> 
            let appAST' = leftAssociate appAST
            (Ok (Funcapp(Funcapp(BuiltInFunc (Math Mult), appAST'), multAST )), rest )
        | (Error msg, rest),_ -> (Error msg, rest)
        | _,(Error msg, rest) -> (Error msg, rest)
    
    | Ok (hd::tl) when hd = DivToken -> 
        let appResult = buildAppExp (Ok acc)
        let multResult = buildMultExp  [] (Ok tl)
        match appResult,multResult with 
        | ((Ok appAST, _),(Ok multAST, rest)) -> 
             let appAST' = leftAssociate appAST
             (Ok (Funcapp(Funcapp(BuiltInFunc (Math Div), appAST'), multAST )), rest )
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


//and (|PBUILDADDEXP|_|) (inp: Result<Token list, Token list>):(Result<AST,string>*Result<Token list, Token list>)option = 

//and (|PBUILDADDEXP|_|) (inp: Result<Token list, Token list>):(Result<AST,string>*Result<Token list, Token list>)option = 
and (|PBUILDMATCHCASES|_|) (inp: Result<Token list, Token list>) : ( (Result<AST,string> list) * Result<Token list, Token list>) option =
    Some <| buildMatchCases inp

and buildMatchCases (inp: Result<Token list, Token list>):(Result<AST,string> list *Result<Token list, Token list>) =
    match inp with
        | Ok x  ->
                       let (cases,res) = split (Keyword "case") x
                       cases
                       |> List.map ( fst << (function |Some x -> x | _ -> failwithf "One case couldn't parse"   )  << ((|PBUILDADDEXP|_|) << Ok)),Ok res
                       //split (Keyword "case") x
                       //|> List.map ( fst << (function |Some x -> x | _ -> failwithf "One case couldn't parse"   )  << ((|PBUILDADDEXP|_|) << Ok)),res
                       //failwithf "A"
        | _ -> failwithf "Cases failed"
    
    
    //Not sure why this fails
    //Option.defaultValue (failwithf "One of the cases couldn't parse")
    
 

and buildAddExp  (acc:Token list) (inp: Result<Token list, Token list>):(Result<AST,string>*Result<Token list, Token list>) =
    match inp with
    | PMATCH (OpenRoundBracket) (TAKEINSIDEBRACKET (Ok(inp', acc')))  ->
        let acc'' = (acc@[OpenRoundBracket] @acc')
        buildAddExp (acc'') inp'


    | PMATCH (OpenSquareBracket) (TAKEINSIDESQBRACKET (Ok(inp', acc')))  ->
        let acc'' = (acc@[OpenSquareBracket] @acc')
        buildAddExp (acc'') inp' 
    
    | PMATCH (Keyword "if") (TAKEINSIDEIF (Ok(inp', acc'))) ->
        let acc'' = (acc@[Keyword "if"] @acc')
        buildAddExp (acc'') inp'
        
    | PMATCH (Keyword "match") (TAKEINSIDEMATCH (Ok(inp', acc'))) ->
        let acc'' = (acc@[Keyword "match"] @ acc')
        buildAddExp (acc'') inp'
    
    | Ok (hd::tl) when hd = AddToken -> 
        let multResult =  buildMultExp [] (Ok acc) 
        let addResult = buildAddExp [] (Ok tl)
        match multResult,addResult with 
        | ((Ok multAST, _),(Ok addAST, rest)) -> (Ok (Funcapp(Funcapp(BuiltInFunc (Math Add), multAST), addAST )), rest )
        | (Error msg, rest),_ -> (Error msg, rest)
        | _,(Error msg, rest) -> (Error msg, rest)
    
    | Ok (hd::tl) when hd = SubToken -> 
        let multResult =  buildMultExp [] (Ok acc) 
        let addResult = buildAddExp [] (Ok tl)
        match multResult,addResult with 
        | ((Ok multAST, _),(Ok addAST, rest)) -> (Ok (Funcapp(Funcapp(BuiltInFunc (Math Sub), multAST), addAST )), rest )
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
        | (Ok ast, Ok []) -> result
        | (Ok ast, rest) -> (Error "Illegal expression at the end", rest )
        | (Error msg, rest) -> (Error msg,  rest)
    | _ -> 
          let res = buildAddExp [] inp
          match res with 
          | (Ok ast, Ok []) -> 
            //let ast' = ast |> leftAssociate
            (Ok ast, Ok [])

          | (Ok ast, rest ) -> (Error "Illegal expression at the end", rest )
          
          | (Error msg, rest) -> (Error msg, rest)


and buildList inp :(Result<AST,string>*Result<Token list, Token list>) = 
    match inp with
    | PBUILDADDEXP (Ok ast, (PMATCH (Keyword ";") (inp'))) -> 
        let result = buildList inp'
        match result with 
        | (Ok ast', rest) ->  (Ok(Pair (ast, ast')), rest)
        | (Error msg, rest) -> (Error msg, rest)
    | PBUILDADDEXP (Ok ast, rest) -> (Ok ast, rest)
    | PBUILDADDEXP (Error msg, rest) -> (Error msg, rest)
    | _ -> failwithf "What? PBUILDADDEXP always matches"

and (|BUILDLIST|_|) inp:(Result<AST,string>*Result<Token list, Token list>) option = Some(buildList inp)
