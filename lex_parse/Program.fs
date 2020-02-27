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
    print <| tokenize_parse "f x y"
    
    0
