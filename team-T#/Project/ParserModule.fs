module ParserModule
open Definitions
    //fst res,snd res
    
 
    
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
    | Ok [] -> Error (sprintf "Missing %A" closingToken)
    | _ -> failwithf "What? Can't happen"

let (|PTAKEINSIDETOKENS|_|) openingToken closingToken inp = Some(takeInsideTokens openingToken closingToken [] inp 1)


let firstOccurrence (x:Token List) (findThis: Token) : int =   
        
    let rec firstOccurenceHelper (x:Token List) (acc:int) : int =
      match x with
       | [] -> -1 // token could not be found
       | hd::tl when hd = findThis -> acc
       | hd::tl -> firstOccurenceHelper tl acc + 1
      
            
    
    firstOccurenceHelper x 0



let split (at:Token) (x:Token list)  : (Token list list * Token list) =
    let rec splitHelper (at:Token) (x:Token list) (acc:Token list) (finalAcc:Token list list) : (Token list list * Token list) =
        match x with
        | [] ->  (finalAcc , acc)
        | hd::tl when hd = Keyword "match"  ->
            match takeInsideTokens (Keyword "match") (Keyword "endmatch") [] (Ok tl) 1 with
            | Ok (mstruct,Ok (_::rest) ) ->
                splitHelper at rest (acc @ [hd] @ mstruct @ [Keyword "endmatch"] ) finalAcc
            | _ -> failwithf "Error while building nested stucture"
        | hd::tl when hd <> at -> splitHelper at tl (acc @ [hd]) finalAcc
        | hd::tl when hd = at ->
            //must be the first time we actually met a split character so we can just split
            splitHelper at tl [] (finalAcc @ [acc])
            | _ -> failwithf "Does this actually happen"
        
    splitHelper at x [] []
    
let sepConditionExpression (x:Token list) : (Token list * Token list) =
    //print x
    let res = split (RightArrow) x
    (fst res).[0], snd res
    //failwithf "Not implemented yet"

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

let (|TAKEINSIDEMATCH|_|) inp = Some(takeInsideMatch [] inp 1)


let (|PMATCH|_|) (tok: Token) (tokLst: Result<Token list, Token list>) = 
    match tokLst with
    | Ok []  -> None
    | Ok (s :: rest) when s = tok -> Some(Ok rest)
    | Ok lst -> None
    | Error lst -> None




let builtInFuncMap = ["mod", BFunc(Mat Mod);"equals", BFunc Equal;"greater",BFunc (Mat Greater);"lower",BFunc (Mat Lower);"explode", BFunc Explode;"implode", BFunc Implode;"pair", BFunc P;"fst", BFunc PFst ;"snd", BFunc PSnd;"ispair", BFunc IsPair; "print", BFunc Print] |> Map.ofList

let rec (|PITEM|_|) (tokLst: Result<Token list, Token list>):(Result<Result<AST,string>*Result<Token list, Token list>,string>) option =
    match tokLst with
    | Ok [] -> Error "Input expression was empty"
    | PMATCH (Other "lazy") (PMATCH (OpenRoundBracket) (PPARSE(result, PMATCH (CloseRoundBracket) (inp')))) -> 
        match result with 
        | Ok ast -> Ok (Ok (Bracket (Lzy ast)), inp')
        | Error msg -> Error msg
    
    | PNOTEXPITEM (Ok(ast, Ok lst)) ->  Ok (ast, Ok lst)
    | PMATCH (OpenRoundBracket) (PMATCH (Other "fun") (BUILDLAMBDA (lamb, PMATCH (CloseRoundBracket) (inp') ) )) ->
        match lamb with 
        | Ok ast -> Ok (lamb, inp')      
        | Error msg -> Error msg


    | PMATCH (OpenRoundBracket) (PPARSE(result, PMATCH (CloseRoundBracket) (inp'))) -> 
        match result with 
        | Ok ast -> Ok (Ok (Bracket ast), inp')
        | Error msg -> Error msg
 
    | PMATCH (Keyword "if") (PTAKEINSIDETOKENS (Keyword "if") (Keyword "then") (Ok (lst, PMATCH (Keyword "then") (PTAKEINSIDETOKENS (Keyword "then") (Keyword "else") (Ok (lst', PMATCH (Keyword "else") (PTAKEINSIDETOKENS (Keyword "else") (Keyword "fi") (Ok (lst'', PMATCH (Keyword "fi") (inp')) ) )) ) )))) ->
         let firstItemParsed = parse (Ok lst)
         let secondItemParsed = parse (Ok lst')
         let thirditemParsed = parse (Ok lst'')
         match firstItemParsed,secondItemParsed,thirditemParsed with 
         | (Ok ast, Ok []),(Ok ast', Ok []),(Ok ast'', Ok []) -> Ok (Ok(Bracket(FuncApp(FuncApp(ast, Lzy (ast')), Lzy (ast'')))), inp')
         | _ -> Error "Couldn't parse one of the expressions in this if statement"

    | PMATCH (OpenSquareBracket) (PBUILDLIST (Ok (ast, PMATCH (CloseSquareBracket) (inp')))) -> Ok (Ok ast, inp')

    | PMATCH (OpenCurlyBracket) (PBUILDMATCHLIST (Ok (ast, PMATCH (CloseCurlyBracket) (inp')))) -> Ok (Ok ast, inp')

    | PMATCH (OpenSquareBracket) (PBUILDLIST (result)) ->
        match result with
        | Ok (ast, PMATCH (CloseSquareBracket) (inp')) -> Ok (Ok ast, inp')
        | Error msg -> Error msg
        | _ -> failwithf "What? Shouldn't happen"


    | PMATCH (Keyword "match") (PBUILDMATCHCASES(astList, Ok A, PMATCH (Keyword "endmatch") (inp')))  ->
        Ok(Ok (MatchDef{Condition = A; Cases = astList}),inp')
    | Ok (hd::tl) -> Error (sprintf "Couldn't parse item %A" hd)
    | Error lst -> Error "Input list was invalid"
    |> Some

and (|PNOTEXPITEM|_|) tokLst = 
    match tokLst with
    | Ok (Other s::rest) when Map.containsKey s builtInFuncMap -> Some(Ok (Ok(builtInFuncMap.[s]) , Ok rest ))
    | Ok (Keyword s::rest) when s = "Y" ->  Some( Ok (Ok Y, Ok rest))
    | Ok (Other s::rest) when s = "true" ->Some( Ok (Ok(Lambda {InputVar=Seq.toList "x";Body=Lambda{InputVar=Seq.toList "y"; Body=Var(Seq.toList"x")} }), Ok rest))
    | Ok (Other s::rest) when s = "false" -> Some(Ok (Ok(Lambda {InputVar=Seq.toList "x";Body=Lambda{InputVar=Seq.toList "y"; Body=Var(Seq.toList "y")} }), Ok rest))
    | Ok (Other s :: rest) -> Some( Ok (Ok(Var(Seq.toList s)), Ok rest))
    | Ok (Other "Null" :: rest) -> Some( Ok (Ok Null, Ok rest))
    | Ok (IntegerLit s:: rest) ->Some( Ok (Ok(Literal (Int s)) ,Ok rest))
    | Ok (StringLit s::rest) ->
        let removeEnds = Seq.toList s |> List.rev |> List.tail |> List.rev |> List.tail
        Some( Ok (Ok(Literal (Str (removeEnds))), Ok rest))
    | _ -> None

and endKeyWordsList = [CloseRoundBracket; CloseSquareBracket; Keyword "then"; Keyword "else"; Keyword "fi"; Keyword ";"; Other "mrec"; Keyword ":"; CloseCurlyBracket]

and buildAppExp(inp: Result<Token list, Token list>):(Result<AST,string>*Result<Token list, Token list>) =
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
    
   | PITEM (Error msg) -> (Error msg, inp)
   | lst -> (Error "Input is not vaid", lst)




and buildMultExp (acc:Token list) (inp: Result<Token list, Token list>) :(Result<AST,string>*Result<Token list, Token list>) = 
    match inp with  
    | PMATCH (OpenRoundBracket) (PTAKEINSIDETOKENS (OpenRoundBracket) (CloseRoundBracket) (result)) -> //(Ok(acc', inp')))  ->
        match result with 
        | Ok (acc', inp') -> 
            let acc'' = (acc@[OpenRoundBracket] @acc')
            buildMultExp (acc'') inp' 
        | Error msg -> (Error msg, inp)
    
    | PMATCH (OpenSquareBracket) (PTAKEINSIDETOKENS (OpenSquareBracket) (CloseSquareBracket) (Ok(acc', inp')))  ->
        let acc'' = (acc@[OpenSquareBracket] @acc')
        buildMultExp (acc'') inp' 

    | PMATCH (OpenCurlyBracket) (PTAKEINSIDETOKENS (OpenCurlyBracket) (CloseCurlyBracket) (Ok(acc', inp')))  ->
        let acc'' = (acc@[OpenCurlyBracket] @acc')
        buildMultExp (acc'') inp' 

    | PMATCH (Keyword "match") (TAKEINSIDEMATCH (Ok(inp', acc'))) ->
        let acc'' = (acc@[Keyword "match"] @ acc')
        buildMultExp (acc'') inp'


    | PMATCH (Keyword "if") (PTAKEINSIDETOKENS (Keyword "if") (Keyword "fi") (result)) ->
        match result with 
        | Ok (acc', inp') ->
            let acc'' = (acc@[Keyword "if"] @acc')
            buildMultExp   (acc'') inp'
        | Error msg -> (Error msg, inp)

    | Ok (hd::tl) when hd = Keyword ";" ->
        match Ok tl with 
        | (PTAKEINSIDETOKENS (OpenSquareBracket) (CloseSquareBracket) (Ok(acc', inp'))) ->
            let acc'' = (acc@[Keyword ";"] @acc')
            buildMultExp (acc'') inp'
        | _ -> (Error "Input list is not valid", inp)

    | Ok (hd::tl) when hd = Keyword ":" ->
        match Ok tl with 
        | (PTAKEINSIDETOKENS (OpenCurlyBracket) (CloseCurlyBracket) (Ok(acc', inp'))) ->
            let acc'' = (acc@[Keyword ":"] @acc')
            buildMultExp (acc'') inp'
        | _ -> (Error "Input list is not valid", inp)

    | Ok (hd::tl) when hd = MultToken -> 
        let appResult = buildAppExp (Ok acc)
        let multResult = buildMultExp  [] (Ok tl)
        match appResult,multResult with 
        | ((Ok appAST, _),(Ok multAST, rest)) -> 
            (Ok (FuncApp(FuncApp(BFunc (Mat Mult), leftAssociate appAST), multAST )), rest )

        | (Error msg, rest),_ -> (Error msg, rest)
        | _,(Error msg, rest) -> (Error msg, rest)
    
    | Ok (hd::tl) when hd = DivToken -> 
        let appResult = buildAppExp (Ok acc)
        let multResult = buildMultExp  [] (Ok tl)
        match appResult,multResult with 
        | ((Ok appAST, _),(Ok multAST, rest)) -> 
             let appAST' = leftAssociate appAST
             (Ok (FuncApp(FuncApp(BFunc (Mat Div), appAST'), multAST )), rest )
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
    match inp with
    | PMATCH (OpenRoundBracket) (PTAKEINSIDETOKENS (OpenRoundBracket) (CloseRoundBracket) (result))  ->
        match result with 
        | Ok (acc', inp') -> 
            let acc'' = (acc@[OpenRoundBracket] @acc')
            buildAddExp (acc'') inp'
        | Error msg -> (Error msg, inp)
    | PMATCH (OpenSquareBracket) (PTAKEINSIDETOKENS (OpenSquareBracket) (CloseSquareBracket) (Ok(acc', inp')))  ->
        let acc'' = (acc@[OpenSquareBracket] @acc')
        buildAddExp (acc'') inp'

    | PMATCH (OpenCurlyBracket) (PTAKEINSIDETOKENS (OpenCurlyBracket) (CloseCurlyBracket) (Ok(acc', inp')))  ->
        let acc'' = (acc@[OpenCurlyBracket] @acc')
        buildAddExp (acc'') inp' 

    | PMATCH (Keyword "match") (TAKEINSIDEMATCH (Ok(inp', acc'))) ->
        let acc'' = (acc@[Keyword "match"] @ acc')
        buildAddExp (acc'') inp'
    
    | PMATCH (Keyword "if") (PTAKEINSIDETOKENS (Keyword "if") (Keyword "fi") (result)) ->
        match result with 
        | Ok (acc', inp') ->
            let acc'' = (acc@[Keyword "if"] @acc')
            buildAddExp (acc'') inp'
        | Error msg -> (Error msg, inp)

    | Ok (hd::tl) when hd = Keyword ";" ->
        match Ok tl with 
        | (PTAKEINSIDETOKENS (OpenSquareBracket) (CloseSquareBracket) (Ok(acc', inp'))) ->
            let acc'' = (acc@[Keyword ";"] @acc')
            buildAddExp (acc'') inp'
        | _ -> (Error "Input list is not valid", inp)

    | Ok (hd::tl) when hd = Keyword ":" ->
        match Ok tl with 
        | (PTAKEINSIDETOKENS (OpenCurlyBracket) (CloseCurlyBracket) (Ok(acc', inp'))) ->
            let acc'' = (acc@[Keyword ":"] @acc')
            buildAddExp (acc'') inp'
        | _ -> (Error "Input list is not valid", inp)
        
    
    | Ok (hd::tl) when hd = AddToken -> 
        let multResult =  buildMultExp [] (Ok acc) 
        let addResult = buildAddExp [] (Ok tl)
        match multResult,addResult with 
        | ((Ok multAST, _),(Ok addAST, rest)) -> (Ok (FuncApp(FuncApp(BFunc (Mat Add), multAST), addAST )), rest )
        | (Error msg, rest),_ -> (Error msg, rest)
        | _,(Error msg, rest) -> (Error msg, rest)
    
    | Ok (hd::tl) when hd = SubToken -> 
        let multResult =  buildMultExp [] (Ok acc) 
        let addResult = buildAddExp [] (Ok tl)
        match multResult,addResult with 
        | ((Ok multAST, _),(Ok addAST, rest)) -> (Ok (FuncApp(FuncApp(BFunc (Mat Sub), multAST), addAST )), rest )
        | (Error msg, rest),_ -> (Error msg, rest)
        | _,(Error msg, rest) -> (Error msg, rest)
    
    | Ok (hd::tl) -> buildAddExp (acc @ [hd]) (Ok tl) 
    | Ok [] -> buildMultExp [] (Ok acc)  
    | Error lst -> (Error "Invalid input expression" , Error lst)

and (|PBUILDADDEXP|_|) (inp: Result<Token list, Token list>):(Result<AST,string>*Result<Token list, Token list>)option = 
    Some (buildAddExp [] inp)

and (|PBUILDMATCHCASES|_|) (inp: Result<Token list, Token list>) : ( (AST * AST) list *Result<AST,string> *Result<Token list, Token list>) option =
    Some <| buildMatchCases inp


and buildMatchCases (inp: Result<Token list, Token list>):( (AST * AST) list * Result<AST , string> * Result<Token list, Token list> ) =
    match inp with
        | Ok x  ->
           
           //Ok Cases
           (*
           let (apply,rest) = cases |> split (Keyword "case")
                        apply
                        |> List.map ( fst << (function |Some x -> x | _ -> failwithf "One case couldn't parse"   )
                        <<
                        ((|PBUILDADDEXP|_|) << Ok)),Ok rest
           *)
           
           let (cases,res) = split (Keyword "case") x
           //print cases.Head
           let toMatch = match (|PBUILDADDEXP|_|) <|  Ok cases.Head with
                            | Some (Ok x, Ok y) when List.isEmpty y -> x
                            | _ -> failwithf "thing to be matched didn't parse"
           
           cases.Tail
           |> List.map
              (
              sepConditionExpression
              >> fun (cond,exp) ->
                 // let minires1 = parse (Ok cond)
                  
                  
                  //match (parse <| Ok cond),(parse <| Ok exp ) with
                  //  | (Ok pCondition,Ok []),(Ok pExpression, Ok []) -> print pCondition; print pExpression ;failwithf "a"
                  //  | _ -> failwithf "A"
                  
                  
                  
                  
                  ( Some <| (parse <| Ok cond)), ( Some <| (parse <| Ok exp))
              >> fun (parsedCondition, parsedExpression) ->
                  match parsedCondition, parsedExpression with
                    | Some (Ok finalCond,Ok resEmpty1), Some (Ok finalExp,Ok resEmpty2) when ((List.isEmpty resEmpty1) && (List.isEmpty resEmpty2) ) -> finalCond, finalExp
                    | _ ->
                        //printf "Parsed Condition %A \n Parsed Expression %A" parsedCondition parsedExpression 
                        failwithf "Parsing before/after arrow in match case failed"
              ) , Ok toMatch,  Ok res
        | _ ->
           failwithf "Cases failed"



and buildLambda (inp:Result<Token list, Token list>):(Result<AST,string>*Result<Token list, Token list>) = 
    //printf "Entered buildLambda with %A \n" inp
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
    | _ -> (Error "insufficient expression" , inp)

and (|BUILDLAMBDA|_|) inp:((Result<AST,string>*Result<Token list, Token list>) option) = Some (buildLambda inp)

and extractParts inp acc: Result<Token list* Token list, string> = 
    match inp with
    | PMATCH (OpenRoundBracket) (PTAKEINSIDETOKENS (OpenRoundBracket) (CloseRoundBracket) (result))  ->
        match result with 
        | Ok (acc', inp') -> 
            let acc'' = (acc@[OpenRoundBracket] @acc')
            extractParts  inp' (acc'')
        | Error msg -> Error msg
    | Ok (hd::tl) when hd = Other "in" -> Ok( acc, tl)
    | Ok (hd::tl) -> extractParts (Ok tl) (acc @ [hd])
    | Ok [] -> Ok (acc, []) //Error "This function definition is never used"
    | _ -> failwithf "What? Can't happen"
and adaptRecursiveExpression lambda  = 
     [Keyword "Y"] @ lambda


and bodyToAnonymousFun inp = 
    [OpenRoundBracket]@[Other "fun"]@inp@[CloseRoundBracket]
    
and  buildFunctionDef inp:(Result<AST,string>*Result<Token list, Token list>)  = 
    match inp with 
    | hd::tl  -> 
        match hd with
            | Other "rec" -> 
                match tl with
                | hd'::tl' ->
                    match hd' with 
                    | Other x -> 
                        let splitFunc = extractParts (Ok tl) []
                        match splitFunc with 
                        | Ok (body, []) -> 
                            let modifiedBody = adaptRecursiveExpression (bodyToAnonymousFun body)
                            match parse (Ok modifiedBody) with
                            | (Ok body, _) -> (Ok(FuncDef(Seq.toList x, body)), Ok [])
                            | (Error msg, rest) -> (Error msg, rest)
                        | Ok (body, expression) ->
                            let modifiedBody = adaptRecursiveExpression (bodyToAnonymousFun body)
                            let parsedExpression = parse (Ok expression)
                            let parsedBody = parse (Ok modifiedBody)
                            match  parsedBody, parsedExpression with 
                            |((Ok body, _),(Ok expression, rest))  -> (Ok (FuncDefExp {Name=Seq.toList x;Body=body; Expression=expression}),rest)
                            | ((Error msg, rest), _) -> (Error msg, rest)
                            | (_, (Error msg, rest)) -> (Error msg, rest)
                        | Error msg -> (Error msg, Error inp)
                    | _ -> (Error  "No name found for this function definition", Error inp)
                | _ -> (Error "Insufficient elements in function definition" , Error inp )

            | Other x -> 
                let splitFunc = extractParts (Ok tl) []
                match splitFunc with 
                | Ok (body, []) -> 
                    match buildLambda (Ok body) with
                    | (Ok body, _) -> (Ok(FuncDef(Seq.toList x, body)), Ok [])
                    | (Error msg, rest) -> (Error msg, rest)
                | Ok (body, expression) ->
                    //printf "Body is %A \n" body
                    let parsedExpression = parse (Ok expression)

                    //printf "\n EXPRESSION IS %A Parsed expression is %A \n" expression parsedExpression
                    //printf "Parsed Expression is %A \n" parsedExpression

                    let parsedBody = buildLambda (Ok body)
                    //printf "Parsed Body is %A \n" parsedBody
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
        buildFunctionDef (rest)

    | PMATCH (Other "mrec") (Ok rest) ->
        let tup = buildMRecExp (rest)
        match tup with 
        | Ok (x, y) -> 
            let res = MutFuncDef (x, y)
            (Ok res, Ok [])
        | Error msg -> (Error msg, inp)
    | _ -> 
        buildAddExp [] inp
 and (|PPARSE|_|) inp = Some(parse inp)

and parsedOutput inp = 
    let res = parse inp
    match res with 
    | (Ok _, Ok []) -> res
    | (Ok _, rest) -> (Error "Ilegal expression at the end", rest )
    | (Error msg, rest) -> (Error msg,  rest)


and (|PBUILDLIST|_|) inp = 
    match inp with 
    | Ok (hd::tl) when hd = CloseSquareBracket -> Ok (Null, Ok (hd::tl))
    | PPARSE (Ok ast, Ok (hd::tl)) -> 
        match hd,(Ok tl) with
        //| Keyword ";", (PPARSE (Ok (ast', lst') ) ) ->
        | Keyword ";", (PBUILDLIST (Ok (ast', lst')))-> Ok (Pair (ast, ast'), lst')
        | CloseSquareBracket, _ -> Ok (Pair (ast, Null), Ok (hd::tl))
        | _ -> Error "Input list is not valid"
    | _ -> Error "Input list is not valid"
    |> Some

and (|PBUILDMATCHLIST|_|) inp = 
    match inp with 
    | Ok (hd::tl) when hd = CloseCurlyBracket -> Ok (Null, Ok (hd::tl))
    | PPARSE (Ok ast, Ok (hd::tl)) -> 
        match hd,(Ok tl) with
        //| Keyword ";", (PPARSE (Ok (ast', lst') ) ) ->
        | Keyword ":", (PBUILDMATCHLIST (Ok (ast', lst')))-> Ok (ExactPairMatch (ast, ast'), lst')
        | CloseCurlyBracket, _ -> Ok (ExactPairMatch (ast, Null), Ok (hd::tl))
        | _ -> Error "Match is not valid"
    | _ -> Error "Match is not valid"
    |> Some

and buildMRecExp inp = 
    match inp with 
    | Other hd::tl ->
        let parsed = buildLambda (Ok tl)
        match parsed with 
        | (Ok ast, Ok ((Other "mrec")::rest)) ->
            let res = buildMRecExp rest
            match res with 
            | Ok (x, y) -> 
                let result = Ok ([Seq.toList hd] @ x, [ast]@y)
                result
            | Error msg -> Error msg
        | (Ok ast, Ok []) ->Ok ([Seq.toList hd], [ast])
        | (Error msg, rest) -> Error msg
        | (Ok ast, Ok ((Other "in")::rest)) -> Error "Write the expression in a separate line"
        | _ -> Error "Invalid mutual recursive functions definition"
    | _ -> Error "Invalid mutual recursive functions definition"

