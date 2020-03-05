open TokenModule
open ParserModule
open Definitions
open Lambdas


let print x =
    printfn "%A" x

let tokenize_parse (x:string) =
    x
    |> tokenize
    |> Ok
    |> parsedOutput
    |> fst

let lambdaEvaluate inp = 
    inp 
    |> tokenize_parse
    |> run






[<EntryPoint>]  
let main argv =
    testsWithExpecto() |> ignore
    //print <| tokenize_parse "[ x + 1 ; y * 2 ]  "
    print <| parsedOutput (Ok [Keyword "if"; Other "x"; Keyword "then"; Other "y"; Keyword "else"; Other"z"; Keyword "fi"])
    //print <| tokenize "match x case y case z case endmatch"
    //print <| lambdaEvaluate "[ 1 ; 2 ; 3 ; 4 ; 5]"
    //print <| lambdaEvaluate "[]"
    //print <| lambdaEvaluate "[[x]]"
    //print <| lambdaEvaluate "let f x = [ [ x ] ; x ] in f 3"
    //print <| lambdaEvaluate "let f x = x + 1 in let g y = y + 2 in f ( g 3 )"
    //print <| tokenize "let f x y = [x ; x * x ; x * x * x ; [ x + y ] ] in f 3 7"
    //print <| tokenize_parse "f x"
    0
