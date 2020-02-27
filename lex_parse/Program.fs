open TokenModule
open ParserModule

let print x =
    printfn "%A" x

let tokenize_parse (x:string) =
    x
    |> tokenize
    |> Ok
    |> parse

[<EntryPoint>]  
let main argv =
    print <| tokenize "let f x = x * 2 in f 2"
    print <| tokenize_parse "let f x = x * 2 in f 2"
    
    0
