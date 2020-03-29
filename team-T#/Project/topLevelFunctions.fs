module TopModule
open Definitions
open TokenModule
open ParserModule
open CombinatorRuntimeModule
open LambdasRuntimeModule
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
    if File.Exists filePath then
        File.ReadAllLines filePath |> Array.toList |> execLines 
    else
        printf "ERROR: File not found"