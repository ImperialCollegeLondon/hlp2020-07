open TokenModule
open ParserModule
open Definitions

let print x =
    printfn "%A" x

let tokenize_parse (x:string) =
    x
    |> tokenize
    |> Ok
    |> parse

[<EntryPoint>]  
let main argv =
    //print <| split (Keyword "case") (tokenize "if case j h l case u case a b c case endmatch")
    //print <| split (Keyword "case") (tokenize "x case 1 case 2 case endmatch f x y")
    //print <| tokenize_parse "match x case match y case f x case f y case endmatch case match f x case 1 case 2 case endmatch case endmatch"
    print <| tokenize_parse "let f x = x + 1 in f 2"
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