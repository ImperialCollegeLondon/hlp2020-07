open TokenModule
open ParserModule
open Definitions
open Lambdas
open System

let print x =
    printfn "%A" x

let tokenize_parse (x:string) =
    x
    |> tokenize
    |> Ok
    |> parsedOutput

let lambdaEval inp = 
    inp 
    |> tokenize_parse
    |> fst
    |> run

let rec FSILike() =
    let input = Console.ReadLine() |> string
    if input = "exit" then () else
        print <| lambdaEval input
        FSILike()

let rec even = fun n -> if n = 0 then true else odd (n-1) 
and odd = fun n -> if n = 0 then false else even (n-1) 


[<EntryPoint>]  
let main argv =
    //FSILike()
    //testsWithExpectoParser() |> ignore
    //print <| parse (Ok [OpenRoundBracket; Keyword "fun"; Other "x"; EqualToken; Other "x"; AddToken; IntegerLit 1L; CloseRoundBracket])
    //print <| run(fst(parse (Ok [Let; Other "rec"; Other "f"; Other "n"; EqualToken; Keyword "if"; Other "equals"; Other "n";
    //IntegerLit 0L; Keyword "then"; IntegerLit 1L; Keyword "else"; Other "n"; MultToken;
    //OpenRoundBracket; Other "f"; OpenRoundBracket; Other "n"; SubToken;
    //IntegerLit 1L; CloseRoundBracket; CloseRoundBracket; Keyword "fi"; Other "in";
    //Other "f"; IntegerLit 3L])))

    //print <| lambdaEval "mrec even n = if equals n 0 then true else odd (n - 1) fi mrec odd n = if equals n 0 then false else even (n - 1) fi"
    //print <| lambdaEval "even 100"
    print <| parsedOutput (Ok [OpenCurlyBracket; Other "hd"; Keyword ":"; Other "tl"; Keyword ":"; Other "rest"; CloseCurlyBracket])


    //print <| run(fst(parse (Ok [Let; Other "rec"; Other "fib"; Other "a"; EqualToken; Keyword "if";
    // Other "equals"; Other "a"; IntegerLit 0L; Keyword "then"; IntegerLit 0L;
    // Keyword "else"; Keyword "if"; Other "equals"; Other "a"; IntegerLit 1L;
    // Keyword "then"; IntegerLit 1L; Keyword "else"; Other "fib"; OpenRoundBracket;
    // Other "a"; SubToken; IntegerLit 1L; CloseRoundBracket; AddToken; Other "fib";
    // OpenRoundBracket; Other "a"; SubToken; IntegerLit 2L; CloseRoundBracket; Keyword "fi";
    // Keyword "fi"; Other "in"; Other "fib"; IntegerLit 9L])))
    //print <| fib 9

    //print <| run (fst (parse (Ok [Let; Other "rec"; Other "f"; Other "p"; EqualToken; Keyword "if"; Other "equals"; Other "p";
    //OpenSquareBracket; CloseSquareBracket; Keyword "then";
    //OpenSquareBracket; CloseSquareBracket; Keyword "else";
    //Other "pair"; OpenRoundBracket; OpenRoundBracket; Other"fst"; Other "p"; CloseRoundBracket; MultToken; IntegerLit 2L;
    //CloseRoundBracket; OpenRoundBracket; Other "f"; OpenRoundBracket; Other "snd"; Other "p"; CloseRoundBracket; CloseRoundBracket;
    //Keyword "fi"; Other "in"; Other "f"; OpenSquareBracket; IntegerLit 1L; Keyword ";"; IntegerLit 2L; MultToken; IntegerLit 3L; Keyword ";"; IntegerLit 3L; CloseSquareBracket])))

    //print <| lambdaEval "let rec f n = if equals n 0 then 1 else n * f (n - 1) fi in let g x = 2*x in g (f 3)"
    //print   <| parse  (Ok [Let; Other "f"; Other "x"; Other "y"; EqualToken; OpenSquareBracket; Other "x";Keyword ";"; Other "x"; MultToken; Other "x"; Keyword ";"; Other "x"; MultToken;Other "x"; MultToken; Other "x"; Keyword ";"; OpenSquareBracket; Other "x";AddToken; Other "y"; CloseSquareBracket; CloseSquareBracket; Other "in";Other "f"; IntegerLit 3L; IntegerLit 7L])
    //print <| split (Keyword "case") (tokenize "if case j h l case u case a b c case endmatch")
    //print <| split (Keyword "case") (tokenize "x case 1 case 2 case endmatch f x y")
    //print <| tokenize_parse "match x case match y case f x case f y case endmatch case match f x case 1 case 2 case endmatch case endmatch"
    //print <| tokenize_parse "match x + 1 case 1 case 2 case endmatch 21 + match x case f case j case endmatch"
    //print <| tokenize_parse "match f + x case match x + 1 case 1 case 2 case endmatch case 1 case endmatch j k"
    //print <| tokenize_parse "f x y"
    
    
    (*
     [Other "f"; AddToken; Other "x"; Keyword "case"; Keyword "match"; Other "x";
       AddToken; IntegerLit 1; Keyword "case"; IntegerLit 1; Keyword "case";
       IntegerLit 2; Keyword "case"; Keyword "endmatch"; Keyword "case";
       Keyword "endmatch"; Other "j"; Other "k"]
    *)
    
    (*print <| split (Keyword "endmatch") [Other "f"; AddToken; Other "x"; Keyword "case"; Keyword "match"; Other "x";
       AddToken; IntegerLit 1; Keyword "case"; IntegerLit 1; Keyword "case";
       IntegerLit 2; Keyword "case"; Keyword "endmatch"; Keyword "case";
       Keyword "endmatch"; Other "j"; Other "k"]
    *)
    
    //let rec takeInsideTokens openingToken closingToken acc inp count = 
    //print <| tokenize "f x 1 case f h 2 case match j case 1 case 2 case endmatch"
    //print <|  takeInsideTokens (Keyword "match") (Keyword "endmatch") [] (Ok <| tokenize "match endmatch if else endmatch match if else endmatch") 1
    //print <| split (Keyword "case") (tokenize "f x case match 3 case 1 case 2 case endmatch case 2 case endmatch f g h ")
    //print <| split (Keyword "case") (tokenize "f x 1 case f 5 + 1 case match x case 1 2 case endmatch case endmatch")    
    
    0
//match AST with \n AST / variable -> AST \n x ->



(*
 match split (Keyword "endmatch") cases with
                        | (x::_,y) when List.isEmpty y ->
                                //not nested should work
                                let (apply,rest) = x @ [Keyword "endmatch"] |> split (Keyword "case")
                                apply
                                |> List.map ( fst << (function |Some x -> x | _ -> failwithf "One case couldn't parse"   )  << ((|PBUILDADDEXP|_|) << Ok)),Ok rest
                        | (x::z,y) ->
                                z
                                x @ [Keyword "endmatch"]
                                //nested how do we deal with it
                                failwithf "We have a nested match"
                        | _ -> failwithf "what? case parsing failed miserably"
*)