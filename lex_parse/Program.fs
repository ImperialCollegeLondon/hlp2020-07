open System
open TokenModule
open ParserModule
open Definitions
open Lambdas
open CombinatorRuntimeModule
//open CombinatorRuntimeModule

let print x =
    printfn "%A" x

let tokenize_parse (x:string) =
    x
    |> tokenize
    |> Ok
    |> parsedOutput
    |> fst

let lambdaEvaluate (inp:string) : Result<AST,string>  = 
    inp 
    |> tokenize_parse
    |> run

let rec FSILike() =
    let input = Console.ReadLine() |> string
    if input = "exit" then () else
        print <| lambdaEvaluate input
        FSILike()

let combinatorEvaluate (inp:string) :Result<AST,string>  = 
    inp 
    |> tokenize_parse
    |> Reduce

[<EntryPoint>]  
let main argv =
    //print <| tokenize "let add1 x = x + 1 in 4"
    //testsWithExpecto() |> ignore
    //print <| tokenize_parse "[ x + 1 ; y * 2 ] "
    //print <| combinatorEvaluate "let f x y = x + y in let g x = f x x in g 8"
    //print <| tokenize "match x case y case z case endmatch"
    //print <| combinatorEvaluate "[ 1 ; 2 ; 3 ; 4 ; 5]"
    //print <| lambdaEvaluate "[]"
    //print <| lambdaEvaluate "[[x]]"
    //print <| lambdaEvaluate "let x = 2 in let y = 1"
    //print <| lambdaEvaluate "let x = 10*2"
    //print <| lambdaEvaluate "let f x = [ [ x ] ; x ] in f 3"
    //print <| lambdaEvaluate "let f x = x + 1 in let g y = y + 2 in f ( g 3 )"
    //print <| combinatorEvaluate "let f x y = [x ; x * x ; x * x * x ; [ x + y ] ] in f 3 7"
    //print <| tokenize_parse "let f x = x + 1 in let g y = y + 2 in f ( g 3 )"
    FSILike()
    Console.ReadKey() |> ignore // not needed running from Visual Studio
    0
