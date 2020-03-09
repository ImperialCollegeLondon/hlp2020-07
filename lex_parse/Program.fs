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

let test1 = [Other "a"; Let; Let; RightArrow; Keyword "a"]

[<EntryPoint>]  
let main argv =
    
    //print <| firstOccurrence [Other "a"; Let; Let; RightArrow; Keyword "a"] Let
    //print <| test1.GetSlice (Some 0, Some (firstOccurrence test1 Let))
    //print <| test1.GetSlice (Some ((firstOccurrence test1 Let) + 1), Some (test1.Length - 1) )
    print <| lambdaEval "match [1;2] case 1 -> 1 + 5 case [1;x;y] -> 5 * 20 case 3 ->  3*100 case endmatch"
    
    //print <| tokenize_parse "match 1 case 1 -> 1 + 5 case [1;2;3] -> 5 * 20 case 3 ->  3*100 case endmatch"
    
    
    //print <| tokenize_parse "pair x (pair a b) "
    //[1;2;3;4] -> Pair (Int 1, Pair (Int 2, Pair (Int 3, Pair(Int 4, Null)) ) )
    
    
    
    //print <| lambdaEval "3 + 1"
    //print <| lambdaEval "match x case 1 case 2 case endmatch + 5"
    0