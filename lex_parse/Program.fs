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

//Need to write more tests here


let bindPairHelperDescriptions =
    [
        (
            "bindPairHelper 1",
            bindPairHelper ( Pair (Literal (Str ['x']), Null)  ) Null [],
            None,
            "when there is nothing to match it should not match"
        )
        (
            "bindPairHelper 2 - Pair",
            bindPairHelper ( Pair (Literal (Int 21L), Null)  ) (Pair (Var ['x'],Null)) [],
            Some [(Pair (Literal (Int 21L),Null), Var ['x'])],
            "one item in 'general' notation should return binded variable as list - one item"
        )
        (
            "bindPairHelper 2 - ExactPairMatch",
            bindPairHelper ( Pair (Literal (Int 21L), Null)  ) (ExactPairMatch (Var ['x'],Null)) [],
            Some [(Literal (Int 21L), Var ['x'])],
            "one item in 'specific' notation should return binded variable correct"
        )
        (
            "bindPairHelper 3 - Pair",
            bindPairHelper ( Pair (Literal (Int 21L), Pair ( Literal (Int 20L), Null  ) )  ) (Pair (Var ['x'],Null)) [],
            Some [(Pair (Literal (Int 21L),Pair (Literal (Int 20L),Null)), Var ['x'])],
            "one item in 'general' notation should return binded variable as list - two items"
        )
        (
            "bindPairHelper 3 - ExactPairMatch",
            bindPairHelper ( Pair (Literal (Int 21L), Pair ( Literal (Int 20L), Null  ) )  ) (ExactPairMatch (Var ['x'],Null)) [],
            None ,
            "no specific match as lists of different sizes"
        )
        (
            "bindPairHelper 4 - Pair",
            bindPairHelper ( Pair (Literal (Int 21L), Pair ( Literal (Int 20L), Null  ) )  ) (Pair (Var ['x'], Pair (Var ['y';'z'],Null)  )) [],
            Some [(Literal (Int 21L), Var ['x']);(Pair (Literal (Int 20L),Null), Var ['y'; 'z'])],
            "First item should be 'specifically' binded (aka the head) and the other variable should match the tail"
        )
        (
            "bindPairHelper 4 - ExactPairMatch",
            bindPairHelper ( Pair (Literal (Int 21L), Pair ( Literal (Int 20L), Null  ) )  ) (ExactPairMatch (Var ['x'], ExactPairMatch (Var ['y';'z'],Null)  )) [],
            Some [(Literal (Int 21L), Var ['x']); (Literal (Int 20L), Var ['y'; 'z'])],
            "Specific match"
        )
        (
            "bindPairHelper 5 - Pair",
            bindPairHelper ( Pair (Literal (Int 21L), Pair ( Literal (Int 20L), Pair (Literal (Int 101L),Null)  ) )  ) (Pair (Var ['x'], Pair (Var ['y';'z'],Null)  )) [],
            Some [(Literal (Int 21L), Var ['x']); (Pair (Literal (Int 20L),Pair (Literal (Int 101L),Null)), Var ['y'; 'z'])],
            "First item should be 'specifically' binded (aka the head) and the other variable should match the tail"
        )
        (
            "bindPairHelper 5 - ExactPairMatch",
            bindPairHelper ( Pair (Literal (Int 21L), Pair ( Literal (Int 20L), Pair (Literal (Int 101L),Null)  ) )  ) (ExactPairMatch (Var ['x'], ExactPairMatch (Var ['y';'z'],Null)  )) [],
            None,
            "Length differs so it should not match"
        )
        (
            "bindPairHelper 6 - ExactPairMatch",
            bindPairHelper ( Pair (Literal (Int 21L), Pair ( Literal (Int 20L), Null  ) )  ) (ExactPairMatch ( Literal (Int 21L), ExactPairMatch (Var ['y';'z'],Null)  )) [],
            Some [(Literal (Int 20L), Var ['y'; 'z'])],
            "Including literals in the cases part"
        )
        
        
    ]

let bindEmptyVariablesDescriptions =
    [
        (
            "bindEmptyVariables 1 Pairs",
            bindEmptyVariables Null,
            [],
            "bindEmptyVariables works for empty list"
        )
        (
            "bindEmptyVariables 2 Pairs",
            bindEmptyVariables (Pair (Var ['x'],Null)),
            [(['x'], Var ['x'])],
            "bindEmptyVariables works for one variable"
        )
        (
            "bindEmptyVariables 3 Pairs",
            bindEmptyVariables (Pair (Var ['x'], Pair ( Var ['y';'z'], Null))),
            [(['x'], Var ['x']);(['y';'z'], Var ['y';'z'])],
            "bindEmptyVariables works for two variables - variable name contains more than 1 character"
        )
        (
            "bindEmptyVariables 1 ExactPairMatch",
            bindEmptyVariables (ExactPairMatch (Var ['x'],Null)),
            [(['x'], Var ['x'])],
            "bindEmptyVariables works for one variable"
        )
        (
            "bindEmptyVariables 2 ExactPairMatch",
            bindEmptyVariables (ExactPairMatch (Var ['x'],Null)),
            [(['x'], Var ['x'])],
            "bindEmptyVariables works for one variable"
        )
        (
            "bindEmptyVariables 3 ExactPairMatch",
            bindEmptyVariables (ExactPairMatch (Var ['x'], ExactPairMatch ( Var ['y';'z'], Null))),
            [(['x'], Var ['x']);(['y';'z'], Var ['y';'z'])],
            "bindEmptyVariables works for two variables - variable name contains more than 1 character"
        )
    ]



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
         lambdaEval "let f = [] in let g = 2 in match f case [x;y;z;a;b;c;d] -> 1 + x + g case [x;y] -> 2 + g + x case [] -> 101 case [x] -> 3  case endmatch",
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
         lambdaEval "let f = [] in let g = [1;2;3;4] in let x = 20 in match f case [] -> g case [x] -> x case [1;2;x] -> 3 case endmatch",
         lambdaEval "[1;2;3;4]",
         "Values and \"lists\" nested match"
        )
        (
         "Values outside nested match pairs 2",
         lambdaEval "let f = [] in let g = [1;2;3;4] in let x = 20 in match f case [] -> match g case [1;2;3;x] -> x case [] -> 3 case endmatch case [x] -> x case [1;2;x] -> 3 case endmatch",
         lambdaEval "[4]",
         "Values and \"lists\" nested match"
        )
        (
            "EDGE CASES 1",
            lambdaEval "let f = [] in let g = [1;2;3;4] in match g case [x;y;z;f;d] -> 100 case [] -> 3 case [x] -> x case endmatch",
            lambdaEval "100",
            "todo"
        )
        (
            "EDGE CASES 2",
            lambdaEval "let f = [] in let g = [1;2;3;4] in match g case [x;y;z;f;d;e] -> 0 case [] -> 1 case [x] -> 100 case endmatch",
            lambdaEval "100",
            "todo"
        )
        (
            "EDGE CASES 3",
            lambdaEval "let f = [] in let g = [1;2;3;4;5] in match g case [x;y] -> match x case 1 -> 1 case endmatch  case [x] -> 1 case endmatch",
            lambdaEval "1",
            "todo"
        )
        (
            "EDGE CASES 4",
            lambdaEval "let f = [] in let g = [1;2;3] in match g case {x:y:z} -> x + y + z case [x] -> 1 case endmatch",
            lambdaEval "6",
            "todo"
        )
        (
            "EDGE CASES 5",
            lambdaEval "let f = [] in let g = [1;2;3] in match g case {x:y} -> x + y case [x;y;z;f] -> x + y + z case endmatch",
            lambdaEval "6",
            "todo"
        )
        (
            "EDGE CASES 6",
            lambdaEval "let f = [] in let g = [1;2;3] in match g case {x:y} -> x + y case [x;y;z;d;f] -> 100 case {x} -> 101 case [x;y] -> 5 case endmatch",
            lambdaEval "5",
            "todo"
        )
        (
            "EDGE CASES 7",
            lambdaEval "let f = [] in let g = [1;2;3] in match g case {x:y} -> x + y case [x;y;z;d;f] -> 100 case {x} -> 101 case [x;y] -> match y case [x] -> 5 case endmatch case endmatch",
            lambdaEval "5",
            "todo"
        )
        (
            "EDGE CASES 8",
            lambdaEval "let f = [] in let g = [1;2;3] in match g case {x:y} -> x + y case [x;y;z;d;f] -> 100 case {x} -> 101 case [x;y] -> match y case {t:u} -> t + u + 3 case endmatch case endmatch",
            lambdaEval "8",
            "todo"
        )
        (
            "EDGE CASES 9",
            lambdaEval "let f = [] in let g = [1] in match g case [] -> 101 case [x;y] -> x case endmatch",
            lambdaEval "1",
            "todo"
        )
        //+ 100 case {} -> 150 case {x} -> x + 10 case
        (
            "EDGE CASES 10",
            lambdaEval "let f = [] in let g = [1] in match g case [] -> 101 case [x;y;z] -> 102 case {x:y} -> x + y case {} -> 150   case [x] -> 1 case endmatch",
            lambdaEval "1",
            "todo"
        )
        (
            "EDGE CASES 11",
            lambdaEval "let f = [] in let g = [1] in match g case [] -> 101 case [x;y;z] -> 102 case {x:y} -> x + y case {} -> 150 case {x} -> x + 10  case [x] -> 1 case endmatch",
            lambdaEval "11",
            "todo"
        )
        (
            "EDGE CASES 12",
            lambdaEval "let f = [] in let g = [1] in match g case [] -> 101 case [x;y;z] -> 102 case [x] -> match x case {l} -> l + 40 case [] -> 500 case endmatch case {x:y} -> x + y case {} -> 150 case {x} -> x + 10  case endmatch",
            lambdaEval "41",
            "todo"
        )
        (
            "EDGE CASES 13",
            lambdaEval "let f = [100] in match f case [x] -> match x case [y] -> match y case [] -> 400 case [z] -> match z case {l} -> l case endmatch case endmatch case endmatch case endmatch",
            lambdaEval "100",
            "todo"
        )
        (
            "EDGE CASES 14",
            lambdaEval "let f = [100] in match f case [x] -> match x case [y] -> match y case [] -> 400 case {c} -> c * 1000 case [z] -> match z case {l} -> l case endmatch case endmatch case endmatch case endmatch",
            lambdaEval "100000",
            "todo"
        )
        (
            "EDGE CASES 15",
            lambdaEval "let o = 0 in let q = [1;1;1;1;1] in match q case [x;y] -> x * o case endmatch",
            lambdaEval "0",
            "todo"
        )
        (
            "EDGE CASES 16",
            lambdaEval "let f = [1;2;3;4;5] in let g = [10;11;12;13;14] in match g case {x:y:z:a:b} -> let o = 0 in let q = [1;1;1;1;1] in match q case [x;y] -> x * o case endmatch case endmatch",
            lambdaEval "0",
            "todo"
        )
        (
         "Specific matches simple 1",
         lambdaEval "let f = [1 ; 2 ; 3 ; 4] in match f case { 1 : 2 : x : y } -> x + y case [] -> [] case endmatch",
         lambdaEval "3 + 4",
         "Variables should now be integer lits as they exact match"
        )
        
        
        
    ]




let makeMyTests (x,y,z,name) = 
      test x {Expect.equal y z name}

[<Tests>]
let bindEmptyVariablesTestGroup = testList "bindEmptyVariables Test Group" (List.map makeMyTests bindEmptyVariablesDescriptions)
[<Tests>]
let matchTestGroup = testList "Match Test Group" (List.map makeMyTests testMatchDescriptions)
[<Tests>]
let bindPairHelperTestGroup = testList "bindPairHelper Test Group" (List.map makeMyTests bindPairHelperDescriptions)
//


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

    //print <| lambdaEval "match [1;2;3;4] case { 1 : 2 : x : y } -> x + y case [] -> 4 case endmatch"
    //print <| lambdaEval "let g = [50;60] in let f = [1;2;10] in match f case { 1 : 2 : x : y } -> x + y case [] -> 4 case [x;y] -> match g case {A : B} -> A + B case endmatch case endmatch"
    runTestsInAssembly defaultConfig [||] |> ignore
    //print <| tokenize_parse "let f = [1;2;3;4;5] in let g = [10;11;12;13;14] in match g case {x:y:z:a:b} -> let o = 0 in let q = [1;1;1;1;1] in match q case [x;y] -> x * o case endmatch case endmatch"
    
    
    (*
    print "-------------------------------"
    print <| tokenize_parse "let o = 0 in let q = [1;1;1;1;1] in match q case [x;y] -> x * o case endmatch"
    print "-------------------------------"
    print <| parse (Ok  [Let; Other "o"; EqualToken; IntegerLit 0L; Other "in"; Let; Other "q";
                                     EqualToken; OpenSquareBracket; IntegerLit 1L; Keyword ";"; IntegerLit 1L;
                                     Keyword ";"; IntegerLit 1L; Keyword ";"; IntegerLit 1L; Keyword ";";
                                     IntegerLit 1L; CloseSquareBracket; Other "in"; Keyword "match"; Other "q";
                                     Keyword "case"; OpenSquareBracket; Other "x"; Keyword ";"; Other "y";
                                     CloseSquareBracket; RightArrow; Other "x"; MultToken; Other "o"; Keyword "case";
                                     Keyword "endmatch"] )
    print "-------------------------------"
    print <| ( (|PBUILDADDEXP|_|) <| Ok  [Let; Other "o"; EqualToken; IntegerLit 0L; Other "in"; Let; Other "q";
                                     EqualToken; OpenSquareBracket; IntegerLit 1L; Keyword ";"; IntegerLit 1L;
                                     Keyword ";"; IntegerLit 1L; Keyword ";"; IntegerLit 1L; Keyword ";";
                                     IntegerLit 1L; CloseSquareBracket; Other "in"; Keyword "match"; Other "q";
                                     Keyword "case"; OpenSquareBracket; Other "x"; Keyword ";"; Other "y";
                                     CloseSquareBracket; RightArrow; Other "x"; MultToken; Other "o"; Keyword "case";
                                     Keyword "endmatch"] )
    
    *)
   
    
    //print <| tokenize_parse "let f = 2 in f * 3 " // let rest = 5 in rest * 20"
    
    
     
    //print <| bindPairHelper ( Pair (Literal (Int 21L), Pair ( Literal (Int 20L), Null  ) )  ) (ExactPairMatch ( Literal (Int 21L), ExactPairMatch (Var ['y';'z'],Null)  )) []
    //print <| lambdaEval "let f = [] in let g = [1] in match g case [] -> 101 case [x;y;z] -> 102 case {x:y} -> x + y case [x] -> 1 case endmatch"  
    //print <| lambdaEval "match [] case [] -> 3 case [x] -> x case [1;2;x] -> 3 case [] -> match [1;2;3;4] case [1;2;3;x] -> x  case endmatch case endmatch"
    //print <| tokenize_parse "{a : b}"
    //print <| tokenize_parse "[]"
    
    
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