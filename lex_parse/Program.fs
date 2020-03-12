open TokenModule
open ParserModule
open Definitions
open Lambdas
open System
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
    runTestsInAssembly defaultConfig [||] |> ignore

  
    
    //print <| split (RightArrow) (tokenize "abc -> match x case 0.1 -> match x case 0 -> 0 case 1 -> 1 case endmatch case 0.3 -> 0.3 case endmatch")
    //print <| lambdaEval "match match 1 case 1 -> 100 case endmatch case 1 -> 1 case 100 -> 49 case endmatch"
    //print <| tokenize_parse "match 1 case 1 -> match 2 case 2 -> 2 case 3 -> 3 case endmatch case endmatch"
    //print <| tokenize_parse "match 1 case 1 -> 2 case 2 -> 3 case endmatch"
    //print <| split (Keyword "case") (tokenize "1 case 1 -> 10 case 2 -> 20 case endmatch")
    //print <| split (Keyword "case") (tokenize "1 case 1 -> match 2 case 2 -> 2 case endmatch case 2 -> 20 case endmatch")

    //print test7
    //print <| split (RightArrow) (tokenize "match 1 case 1 -> match 2 case 2 -> 2 case 3 -> 3 case endmatch case endmatch")
    //print <| lambdaEval "match 1 case 1 -> match 2 case 2 -> match 3 case 3 -> 71 case endmatch case 3 -> 3 case endmatch case endmatch"
                        //"match 1 case 1 -> 1 case endmatch"
    
    // 1
    // match 2 case 2 -> 2
    // match 1  -> 2 case 2 -> 2 case endmatch
    
    //print <| firstOccurrence [Other "a"; Let; Let; RightArrow; Keyword "a"] Let
    //print <| test1.GetSlice (Some 0, Some (firstOccurrence test1 Let))
    //print <| test1.GetSlice (Some ((firstOccurrence test1 Let) + 1), Some (test1.Length - 1) )
    
    //print <| bindEmptyVariables test5
    
    
    
    
    
    //print <| lambdaEval "let h = 2 in let f = 1 in f * h"
    //print <| lengthPair test2
    //print <| bindPair test3 [test4]
    //print <| tokenize_parse "match 1 case 1 -> 1 + 5 case [1;2;3] -> 5 * 20 case 3 ->  3*100 case endmatch"
    
    
    //print <| tokenize_parse "pair x (pair a b) "
    //[1;2;3;4] -> Pair (Int 1, Pair (Int 2, Pair (Int 3, Pair(Int 4, Null)) ) )
    
    
    
    //print <| lambdaEval "3 + 1"
    //print <| lambdaEval "match x case 1 case 2 case endmatch + 5"
    0