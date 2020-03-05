module ParserModule
open Expecto
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






let builtInFuncMap = ["mod", BFunc(Mat Mod);"equals", BFunc Equal;"explode", BFunc Explode;"implode", BFunc Implode;"pair", BFunc P;"fst", BFunc PFst ;"snd", BFunc PSnd;"ispair", BFunc IsPair] |> Map.ofList

let rec (|PITEM|_|) (tokLst: Result<Token list, Token list>):(Result<Result<AST,string>*Result<Token list, Token list>,string>) option =
    match tokLst with
    | Ok [] -> Error "Input expression was empty"
    | PNOTEXPITEM (Ok(ast, Ok lst)) ->  Ok (ast, Ok lst)
    | PMATCH (OpenRoundBracket) (PMATCH (Keyword "fun") (BUILDLAMBDA (lamb, PMATCH (CloseRoundBracket) (inp') ) )) ->
        match lamb with 
        | Ok ast -> Ok (lamb, inp')      
        | Error msg -> Error msg
    
    | PMATCH (OpenRoundBracket) (PBUILDADDEXP(result, PMATCH (CloseRoundBracket) (inp'))) -> 
        match result with 
        | Ok ast -> Ok (Ok (Bracket ast), inp')
        | Error msg -> Error msg
 
    | PMATCH (Keyword "if") (PTAKEINSIDETOKENS (Keyword "if") (Keyword "then") (Ok (lst, PMATCH (Keyword "then") (PTAKEINSIDETOKENS (Keyword "then") (Keyword "else") (Ok (lst', PMATCH (Keyword "else") (PTAKEINSIDETOKENS (Keyword "else") (Keyword "fi") (Ok (lst'', PMATCH (Keyword "fi") (inp')) ) )) ) )))) ->
         let firstItemParsed = parse (Ok lst)
         let secondItemParsed = parse (Ok lst')
         let thirditemParsed = parse (Ok lst'')
         match firstItemParsed,secondItemParsed,thirditemParsed with 
         | (Ok ast, Ok []),(Ok ast', Ok []),(Ok ast'', Ok []) -> Ok (Ok(Bracket(FuncApp(FuncApp(ast, Lazy (ast')), Lazy (ast'')))), inp')
         | _ -> Error "Couldn't parse one of the expressions in this if statement"

    | PMATCH (OpenSquareBracket) (PBUILDLIST (Ok (ast, PMATCH (CloseSquareBracket) (inp')))) -> Ok (Ok ast, inp')

    | PMATCH (OpenSquareBracket) (PBUILDLIST (result)) ->
        match result with
        | Ok (ast, PMATCH (CloseSquareBracket) (inp')) -> Ok (Ok ast, inp')
        | Error msg -> Error msg


    | PMATCH (Keyword "match") (PBUILDMATCHCASES(astList, PMATCH (Keyword "endmatch") (inp')))  ->
        //print ast
        
        let res = List.map (function |Ok x -> x |y -> failwithf "One of the cases failed") astList
        //Assumes at least one
        //Not dealt with errors yet
        Ok(Ok (MatchDef{Condition = res.Head; Cases = res.Tail}),inp')
    | Ok (hd::tl) -> Error (sprintf "Couldn't parse item %A" hd)
    | Error lst -> Error "Input list was invalid"
    |> Some

and (|PNOTEXPITEM|_|) tokLst = 
    match tokLst with
    | Ok (Other s::rest) when Map.containsKey s builtInFuncMap -> Some(Ok (Ok(builtInFuncMap.[s]) , Ok rest ))
    | Ok (Other s::rest) when s = "TRUE" ->Some( Ok (Ok(Lambda {InputVar=Seq.toList "x";Body=Lambda{InputVar=Seq.toList "y"; Body=Var(Seq.toList"x")} }), Ok rest))
    | Ok (Other s::rest) when s = "FALSE" -> Some(Ok (Ok(Lambda {InputVar=Seq.toList "x";Body=Lambda{InputVar=Seq.toList "y"; Body=Var(Seq.toList "y")} }), Ok rest))
    | Ok (Other s :: rest) -> Some( Ok (Ok(Var(Seq.toList s)), Ok rest))
    | Ok (Other "Null" :: rest) -> Some( Ok (Ok Null, Ok rest))
    | Ok (IntegerLit s:: rest) ->Some( Ok (Ok(Literal (Int s)) ,Ok rest))
    | Ok (StringLit s::rest) ->Some( Ok (Ok(Literal (Str (Seq.toList s))), Ok rest))
    | _ -> None

and endKeyWordsList = [CloseRoundBracket; CloseSquareBracket; Keyword "then"; Keyword "else"; Keyword "fi"; Keyword ";"]

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
    | [] -> Ok (acc, []) //Error "This function definition is never used"
    
and  buildFunctionDef inp:(Result<AST,string>*Result<Token list, Token list>)  = 
    match inp with 
    | hd::tl  -> 
        match hd with 
            | Other x -> 
                let splitFunc = extractParts tl []
                match splitFunc with 
                | Ok (body, []) -> 
                    match buildLambda (Ok body) with
                    | (Ok body, _) -> (Ok(FuncDef(Seq.toList x, body)), Ok [])
                    | (Error msg, rest) -> (Error msg, rest)
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
        buildFunctionDef (rest)
        
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


let makeTests (name, instr, outI) =
    test name {
        let tokLst = instr
        match (tokLst |> parsedOutput |> fst) with 
        | Ok ast -> Expect.equal (Ok ast) outI (sprintf "%A" tokLst)
        | Error msg -> Expect.equal (Error msg) outI (sprintf "%A" tokLst)
    }
let testListWithExpecto = 
    [
        "first test",(Ok [Other "x"; Other "y"]),(Ok(FuncApp(Var ['x'], Var ['y'])))
        "second test",(Ok [Keyword "if"; Other "x";  IntegerLit 1L; Keyword "then"; Other "y"; IntegerLit 1L; Keyword "else"; Other "z"; IntegerLit 1L; Keyword "fi"]) ,(Ok(FuncApp(FuncApp(FuncApp (Var ['x'],Literal (Int 1L)),Lazy (FuncApp (Var ['y'],Literal (Int 1L)))),Lazy (FuncApp (Var ['z'],Literal (Int 1L))))))
        "third test", (Ok [Let; Other "x"; EqualToken; Other "x"; MultToken; IntegerLit 2L; Other "in"; Other "x"; IntegerLit 5L]), (Ok(FuncDefExp{ Name = ['x'];Body = FuncApp (FuncApp (BFunc (Mat Mult),Var ['x']),Literal (Int 2L));Expression = FuncApp (Var ['x'],Literal (Int 5L)) }))   
        "fourth test", (Ok [OpenSquareBracket; CloseSquareBracket; Other "x"; Other"y"]), Ok (FuncApp (FuncApp (Null,Var ['x']),Var ['y']))
        "fifth test", (Ok [Let; Other "f"; Other "x"; EqualToken; Other "x"; MultToken; IntegerLit 2L]), (Error "This function definition is never used")
        "sixth test", (Ok [Let; Other "x"; EqualToken; Other "x"; MultToken; IntegerLit 2L; Other "in"; Other "x"; IntegerLit 5L]), (Ok(FuncDefExp{ Name = ['x'];Body = FuncApp (FuncApp (BFunc (Mat Mult),Var ['x']),Literal (Int 2L));Expression = FuncApp (Var ['x'],Literal (Int 5L)) }))
        "seventh test", (Ok [OpenRoundBracket; Keyword "fun"; Other "x"; EqualToken; Other "x"; AddToken; IntegerLit 1L; CloseRoundBracket]), (Ok(Lambda{ InputVar = ['x'];Body = FuncApp (FuncApp (BFunc (Mat Add),Var ['x']),Literal (Int 1L)) }))
        "eigth test", (Ok [Other "f"; OpenRoundBracket; Other "x"; Other "y";Other "z"; CloseRoundBracket]), Ok (FuncApp (Var ['f'],FuncApp (FuncApp (Var ['x'],Var ['y']),Var ['z'])))
        "ninth test", Ok [Other "f"; OpenRoundBracket; Other "a"; OpenRoundBracket; Other "f"; Other "d"; CloseRoundBracket; CloseRoundBracket], Ok (FuncApp (Var ['f'],FuncApp (Var ['a'],FuncApp (Var ['f'],Var ['d']))))
        "tenth test", Ok [OpenSquareBracket; OpenSquareBracket; Other "x"; CloseSquareBracket], Error "Input list is not valid"
        "eleventh test", Ok([Let; Other "f"; Other "x"; Other"y"; EqualToken; Other "x"; Other "in"; Other "f"; IntegerLit 3L; IntegerLit 4L]), Ok(FuncDefExp{ Name = ['f'];Body = Lambda { InputVar = ['x'];Body = Lambda { InputVar = ['y'];Body = Var ['x'] } };Expression =FuncApp(FuncApp (Var ['f'],Literal (Int 3L)),Literal (Int 4L)) })
        "test 12", Ok [OpenSquareBracket; Other "x";  IntegerLit 1L; Keyword ";"; Other "y"; IntegerLit 2L; Keyword ";"; Other "z"; IntegerLit 3L; CloseSquareBracket ], Ok(Pair(FuncApp (Var ['x'],Literal (Int 1L)),Pair(FuncApp (Var ['y'],Literal (Int 2L)),Pair (FuncApp (Var ['z'],Literal (Int 3L)),Null))))
        "test 13", Ok [OpenSquareBracket; Other "x"; AddToken; IntegerLit 1L; Keyword ";"; Other "y"; MultToken; IntegerLit 2L; CloseSquareBracket ], Ok(Pair(FuncApp (FuncApp (BFunc (Mat Add),Var ['x']),Literal (Int 1L)),Pair (FuncApp (FuncApp (BFunc (Mat Mult),Var ['y']),Literal (Int 2L)),Null)))
        "test 14", Ok [Other "f";MultToken; Other "g";MultToken; Other "h";AddToken; IntegerLit 1L], Ok(FuncApp(FuncApp(BFunc (Mat Add),FuncApp(FuncApp (BFunc (Mat Mult),Var ['f']),FuncApp (FuncApp (BFunc (Mat Mult),Var ['g']),Var ['h']))),Literal (Int 1L)))
        "test 15", Ok [Other"h";Keyword "if"; Other "x"; Keyword "then"; Other "y";  Keyword "else"; Other "z";  Keyword "fi"; Other"f"], Ok(FuncApp(FuncApp(Var ['h'],FuncApp (FuncApp (Var ['x'],Lazy (Var ['y'])),Lazy (Var ['z']))),Var ['f']))
        "test 16", Ok [Let; Other "f"; Other "x"; Other "y"; EqualToken; OpenSquareBracket; Other "x";Keyword ";"; Other "x"; MultToken; Other "x"; Keyword ";"; Other "x"; MultToken;Other "x"; MultToken; Other "x"; Keyword ";"; OpenSquareBracket; Other "x";AddToken; Other "y"; CloseSquareBracket; CloseSquareBracket; Other "in";Other "f"; IntegerLit 3L; IntegerLit 7L], (Ok
        (FuncDefExp
           { Name = ['f']
             Body =
                   Lambda
                     { InputVar = ['x']
                       Body =
                             Lambda
                               { InputVar = ['y']
                                 Body =
                                       Pair
                                         (Var ['x'],
                                          Pair
                                            (FuncApp
                                               (FuncApp (BFunc (Mat Mult),Var ['x']),
                                                Var ['x']),
                                             Pair
                                               (FuncApp
                                                  (FuncApp
                                                     (BFunc (Mat Mult),Var ['x']),
                                                   FuncApp
                                                     (FuncApp
                                                        (BFunc (Mat Mult),Var ['x']),
                                                      Var ['x'])),
                                                Pair
                                                  (Pair
                                                     (FuncApp
                                                        (FuncApp
                                                           (BFunc (Mat Add),Var ['x']),
                                                         Var ['y']),Null),Null)))) } }
             Expression =
                         FuncApp (FuncApp (Var ['f'],Literal (Int 3L)),Literal (Int 7L)) }))
    ]  
    |> List.map makeTests
    |> testList "Set of tests"

let testsWithExpecto() =
    runTests defaultConfig testListWithExpecto |> ignore