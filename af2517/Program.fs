// Learn more about F# at http://fsharp.org

open System
//If FSI not finding the module do in the FSI #load ParserModule
open ParserModule
let print x = printfn "%A" x

[<EntryPoint>]
let main argv =
    //print <| BuildAddExp (Ok [Other ['f']; Other['x']; Other ['y']]) []
    print <| CheckForLet (Ok [Other ['2']; Other['*']; Other['3']; Other['+']; Other['4']])
    print <| CheckForLet (Ok [Other ['m';'o';'d']; IntToken 4; IntToken 2])
    //let a = ExtractRightAppList [] (Funcapp(Var ['f'], Funcapp(Var ['x'],Var ['y'])))
   // let b = List.rev a
    //print <| MakeLeftAppList b
    //print <| BuildAddExp (Ok [Other ['f']; Other['x']; Other ['y']]) []
    //print <| BuildAddExp (Ok [Other ['f']; Bracket['(']; Other['x']; Other ['y']; Bracket[')']]) []
    print <| CheckForLet (Ok([Keyword LET; Other ['f']; Other['x']; Keyword EQUAL; Other['x']; Other['+']; IntToken 1; Keyword IN; Keyword LET; Other ['g']; Other['y']; Keyword EQUAL; Other['y']; Other['+']; IntToken 2;Keyword IN; Other ['g']; Other['f'];IntToken 3] ) )
    //print <| BuildAddExp (Ok [Other ['f']; Bracket['(']; Other['x']; Other ['y']; ]) []
    0 // return an integer exit code
    