// Learn more about F# at http://fsharp.org

open System
//If FSI not finding the module do in the FSI #load ParserModule
open ParserModule


[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    test()
    0 // return an integer exit code
