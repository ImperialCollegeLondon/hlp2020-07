// Learn more about F# at http://fsharp.org

open System
//If FSI not finding the module do in the FSI #load ParserModule
open ParserModule
let print x = printfn "%A" x

[<EntryPoint>]
let main argv =
    //let a = BuildAppExp (Ok [Other ['f']; Other ['g']; Other['h']])
    //let b = FlattenAST [] (fst a)
    //print <| a
    //print <| b
    //print <| ReverseAST b

    //print <| BuildMultExp (Ok [Other ['f']; Other ['g']; Other['h']]) []
    //print <| BuildMultExp (Ok [Other ['f']; Other ['g']; Other['h']; Other['*'];Other ['f']; Other ['g']; Other['h']; Other['*'];Other ['f']; Other ['g']; Other['h']]) []
    //print <| BuildMultExp (Ok [Other ['f']; Other ['g']; Other['h']; Other['*'];Other ['f']; Other ['g']; Other['h']]) []
    print <| BuildAddExp (Ok [Other ['f']; Other['*']; Other['g']; Other['+']; Other['h']; Other['p']]) []
    0 // return an integer exit code



