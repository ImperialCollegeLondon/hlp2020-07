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
    //print <| tokenize "let f x = x * 2 in f 2"
    //print <| (fst <| tokenize_parse "let f x = x * 2 in f 2")
    //print <| (fst <| tokenize_parse "let f x = x in let g x = x in g f 21")
    print <| (fst <| tokenize_parse "let f x y = x * y * 2 + 1 in let g y = y in f 2 (g 5)")
    //print <| buildAddExp [] (Ok(tokenize "x * y * 2 + 1 "))
    //print <| (fst <| tokenize_parse "[1 ; 2 ; 3 ; 4 ; 5 ; 6]")
    //print <| tokenize "\n"
    //print <| parse "match "
    //print <| tokenize_parse " match "
    0
//match AST with \n AST / variable -> AST \n x -> 