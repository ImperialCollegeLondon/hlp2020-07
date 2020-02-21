// Learn more about F# at http://fsharp.org

open System
//If FSI not finding the module do in the FSI #load ParserModule
open ParserModule
let print x = printfn "%A" x

[<EntryPoint>]
let main argv =
    print <| BuildAddExp (Ok [Other ['f']; Other['x']; Other ['y']]) []
    print <| BuildAddExp (Ok [Other ['2']; Other['*']; Other['3']; Other['+']; Other['4']]) []
    let a = ExtractRightAppList [] (Funcapp(Var ['f'], Funcapp(Var ['x'],Var ['y'])))
    let b = List.rev a
    print <| MakeLeftAppList b
    0 // return an integer exit code
    