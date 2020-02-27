open TokenModule
open ParserModule

let print x =
    printfn "%A" x



[<EntryPoint>]  
let main argv =
    let res = tokenize "f x y"
    print res
    print <| parse (Ok res)
    
    0
