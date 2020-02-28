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
    print <| tokenize_parse "match x x x x case f j k case add f s j 2 case endmatch j k"
    print <| tokenize_parse "f x y"
    
    
    0
//match AST with \n AST / variable -> AST \n x -> 