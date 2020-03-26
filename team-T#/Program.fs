open TokenModule
open ParserModule
open CombinatorRuntimeModule
open Definitions
open LambdasRuntimeModule
open System
open System.IO
open Expecto


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

let combinatorEval inp =
    let exp = inp |> tokenize_parse |> fst |> Reduce
    print <| "\n\n\n"
    print <| exp
    printfn "This is the result of the function : %A" inp
    print <| "\n\n\n"
    exp

let (|COMMENT|_|) = 
    function
    | hd::tl ->
        hd 
        |> Seq.toList 
        |> function 
        | '/'::'/'::_ -> Some tl 
        | _ -> None
    | _ -> None

let execFile runtime (filePath) =
    let rec execLines =
        function
        | ""::tl -> execLines tl
        | COMMENT tl -> execLines tl
        | hd::tl -> 
            match runtime hd with
            | Error(err) -> print err 
            | Ok(_) -> execLines tl
        | [] -> ()
    File.ReadAllLines filePath |> Array.toList |> execLines

//Need to write more tests here


(*let bindPairHelperDescriptions =
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
        
        
let testLambdaEvalDescriptions =
    [
        (
         "Lambda 1",
         lambdaEval "let rec fac n = if equals n 0 then 1 else n * fac ( n - 1 ) fi in fac 5",
         Ok (Literal (Int 120L)),
         "Factorial function"
        )
        (
         "Lambda 2",
         lambdaEval "let g f x = f x x in g ( fun x y = x * y ) 3",
         Ok (Literal (Int 9L)),
         "Anonymus function as argument"
        )   
        (
         "Lambda 3",
         lambdaEval "let rec fib n = if equals n 1 then 1 else if equals n 2 then 1 else fib ( n - 1 ) + fib ( n - 2 ) fi fi in fib 10",
         Ok (Literal (Int 55L)),
         "Fibonacci function"
        )
        (
         "Lambda 4",
         lambdaEval "let rec lstmap func lst = if equals lst [ ] then [ ] else pair ( func ( fst lst ) ) ( lstmap func ( snd lst ) ) fi in lstmap ( fun x = x * 2 ) [ 1 ; 2 ; 3 ] ",
         Ok (Pair(Literal(Int 2L),Pair(Literal(Int 4L),Pair(Literal(Int 6L),Null)))),
         "list.Map function"
        )
        (
         """Lambda 5: reverseword "abcdf" """,
         lambdaEval """let rec concatlist lsta lstb = if equals lsta [] then lstb else pair ( fst lsta ) ( concatlist ( snd lsta ) lstb )  fi in let rec reverselist lst = if equals lst [] then [] else concatlist ( reverselist ( snd lst ) ) ( [ fst lst ] ) fi in let reverseword str = implode ( reverselist ( explode str ) ) in reverseword "abcdef" """,
         Ok (Literal (Str ['f'; 'e'; 'd'; 'c'; 'b'; 'a'])),
         "Concatenate, reverselist, reverseword"
        )
        (
         "Lambda 6: mutual recursion",
         [lambdaEval "mrec even n = if equals n 0 then true else odd ( n - 1 ) fi mrec odd n = if equals n 0 then false else even ( n - 1 ) fi" ; lambdaEval "[ even 101 ; odd 100 ; even 30 ; odd 31 ]"].[1],
         Ok (Pair(falseAST,Pair(falseAST,Pair(trueAST,Pair(trueAST,Null))))),
         "Even and odd functions"
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
[<Tests>]
let lambdaEvalTestGroup = testList "Lambda Test Group" (List.map makeMyTests testLambdaEvalDescriptions)

*)

[<EntryPoint>]  
let main argv =
    // THE FOLLOWING FILE IS TO RUN WITH THE LAMBDA RUNTIME
    testsWithExpectoParser() |> ignore
    testsWithExpectoLambdaRuntime() |> ignore
    execFile lambdaEval (Environment.CurrentDirectory + "/demoLamb.TSHARP")
    // THE FOLLOWING FILE IS TO RUN WITH THE COMBINATOR RUNTIME
    // execFile combinatorEval ("/Users/elliott/F#/hlp2020-07/lex_parse/demoComb.TSHARP")
    Console.ReadKey() |> ignore
    


    //runTestsInAssembly defaultConfig [||] |> ignore
    
    0