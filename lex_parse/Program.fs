open TokenModule
open ParserModule
open Definitions
open Lambdas
open System
open System.IO
open Expecto

let print x =
    printfn "%A" x


let tokenize_parse (x:string) =
    x
    |> tokenize
    |> Ok
    |> parse

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

let printASTResult = 
    function
    | Literal(Int n) -> n |> print
    | Literal(Str cLst) -> String.Concat(Array.ofList(cLst)) |> print
    | _ -> print "\n"


let execFile(filePath) =
    let rec execLines =
        function
        | hd::tl -> 
            match lambdaEval hd with
            | Error(err) -> print err 
            | Ok(ast) -> printASTResult ast; execLines tl
        | [] -> ()
    File.ReadAllLines filePath |> Array.toList |> execLines


let rec even = fun n -> if n = 0 then true else odd (n-1) 
and odd = fun n -> if n = 0 then false else even (n-1) 



let testMatchDescriptions =
    [
        (
         "Values Defined outside 1",
         lambdaEval "let f = [1;2;3;4;5] in let g = 2 in match f case [x;y;z;a;b;c;d] -> 1 + x + g case [x;y] -> 2 + g + x case [x] -> 3 case endmatch",
         Ok (Literal (Int 5L)),
         "Pair defined as value outside\nCase uses value defined outside\n"
        )
        (
         "Values Defined outside 2",
         lambdaEval "let f = [] in let g = 2 in match f case [x;y;z;a;b;c;d] -> 1 + x + g case [x;y] -> 2 + g + x case [x] -> 3 case [] -> 101 case endmatch",
         Ok (Literal (Int 101L)),
         "Empty list should be empty"
        )
        (
         "Values Defined outside 3",
         lambdaEval "let f = [1;2;3] in let g = 2 in let j = 21 in match f case [x;y;z;a;b;c;d] -> 1 + x + g case [x;y] -> j * g case [x] -> 3 case [] -> 101 case endmatch",
         Ok (Literal (Int 42L)),
         "Shadowing cases by having more generic ones before should work"
        )
        (
         "Values Defined outside 4",
         lambdaEval "let f = [1;2;3] in let g = 2 in let j = 21 in match f case [x;y;z] -> x + y case [x;y] -> j * g case [x] -> 3 case [] -> 101 case endmatch",
         Ok (Literal (Int 3L)),
         "Just enough variables for all but the last element"
        )
        (
         "Values Defined outside 5",
         lambdaEval "let f = 10 in let g = 2 in let j = 21 in match f * g * j case 419 -> j * j case 421 -> g * g * g * g case 420 -> f * f * f case endmatch",
         Ok (Literal (Int 1000L)),
         "Condition of match is itself a FuncApp"
        )
        (
         "Simple Match 1",
         lambdaEval "match 1 case 1 -> 1 case endmatch",
         Ok (Literal (Int 1L)),
         "Single case"
        )
        (
         "Simple Match 2",
         lambdaEval "match 1 case 2 -> 2 case 1 -> 1 case endmatch",
         Ok (Literal (Int 1L)),
         "Double case"
        )
        (
         "Nested Match return value 1",
         lambdaEval "match 1 case 1 -> match 2 case 2 -> match 3 case 3 -> 71 case endmatch case 3 -> 3 case endmatch case endmatch",
         Ok (Literal (Int 71L)),
         "Only values nested match"
        )
        (
         "Nested Match return value 2",
         lambdaEval "match 1 case 3 -> 49 case 1 ->  match [1;2;3;4] case [] -> 1 case [x;y] -> x + 5 case endmatch case endmatch",
         Ok (Literal (Int 6L)),
         "Values and \"lists\" nested match"
        )
        (
         "Nested Match return value 3",
         lambdaEval "match 1 case 3 -> 49 case 1 ->  match [1;2;3;4] case [] -> 1 case [x;y;z] -> x + y * 20 case endmatch case endmatch",
         Ok (Literal (Int 41L)),
         "Values and \"lists\" nested match"
        )
        (
         "Nested Match return pair 1",
         lambdaEval "match 1 case 3 -> 49 case 1 ->  match [1;2;3;4] case [] -> 1 case [x;y;z] -> z case endmatch case endmatch",
         lambdaEval "[3;4]",
         "Values and \"lists\" nested match"
        )
        (
         "Nested Match return pair 2",
         lambdaEval "match 1 case 3 -> 49 case 1 ->  match [1;2;3;4] case [] -> 1 case [x;y;z] -> [] case endmatch case endmatch",
         lambdaEval "[]",
         "Values and \"lists\" nested match"
        )
        (
         "Nested Match return pair 3",
         lambdaEval "match 1 case 3 -> 49 case 1 ->  match [] case [] -> 1 case [x;y;z] -> [] case endmatch case endmatch",
         lambdaEval "1",
         "Values and \"lists\" nested match"
        )
        (
         "Values outside nested match pairs 1",
         lambdaEval "let f = [] in let g = [1;2;3;4] in let x = 20 in match f case [x] -> x case [1;2;x] -> 3 case [] -> g case endmatch",
         lambdaEval "[1;2;3;4]",
         "Values and \"lists\" nested match"
        )
        (
         "Values outside nested match pairs 2",
         lambdaEval "let f = [] in let g = [1;2;3;4] in let x = 20 in match f case [x] -> x case [1;2;x] -> 3 case [] -> match g case [1;2;3;x] -> 1 case [] -> 3 case endmatch case endmatch",
         lambdaEval "1",
         "Values and \"lists\" nested match"
        )
        (
         "Values outside nested match pairs 3",
         lambdaEval "let f = [] in let g = [1;2;3;4] in let x = 20 in match f case [x] -> x case [1;2;x] -> 3 case [] -> match g case [1;2;3;x] -> x case [] -> 3 case endmatch case endmatch",
         lambdaEval "[4]",
         "Values and \"lists\" nested match"
        )
        (
         "Values outside nested match pairs 4",
         lambdaEval "let f = [] in let g = [1;2;3;4] in let x = 20 in match f case [x] -> x case [1;2;x] -> 3 case [] -> match g case [3;2;1;x] -> x case [] -> 3 case [1;2;x] -> x case endmatch case endmatch",
         lambdaEval "[3;4]",
         "Values and \"lists\" nested match"
        )
        
        
    ]
let makeMyTests (x,y,z,name) = 
      test x {Expect.equal y z name}
[<Tests>]
let matchTestGroup = testList "Match Test Group" (List.map makeMyTests testMatchDescriptions)






[<EntryPoint>]  
let main argv =
    //execFile("C:\\Users\\danig\\Desktop\\myF#\\hlp2020-07\\lex_parse\\demo.THARP")
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
    
    
    //print <| parsedOutput (Ok [OpenCurlyBracket; Other "hd"; Keyword ":"; Other "tl"; Keyword ":"; Other "rest"; CloseCurlyBracket])


    runTestsInAssembly defaultConfig [||] |> ignore

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
    
    
    //print <| tokenize_parse "pair x (pair a b) "
    //[1;2;3;4] -> Pair (Int 1, Pair (Int 2, Pair (Int 3, Pair(Int 4, Null)) ) )
    
    
    
    //print <| lambdaEval "3 + 1"
    //print <| lambdaEval "match x case 1 case 2 case endmatch + 5"
    0