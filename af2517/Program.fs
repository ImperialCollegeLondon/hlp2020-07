﻿// Learn more about F# at http://fsharp.org

open System
//If FSI not finding the module do in the FSI #load ParserModule
open ParserModule
let print x = printfn "%A" x

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    print <| PappExp (Ok [Other ['f']; Other ['g']; Other['h']])
    test()
    0 // return an integer exit code



