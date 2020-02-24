// Learn more about F# at http://fsharp.org

open System
//If FSI not finding the module do in the FSI #load ParserModule
open ParserModule
let print x = printfn "%A" x

[<EntryPoint>]
let main argv =
    //print <| BuildAddExp (Ok [Other ['f']; Other['x']; Other ['y']]) []
   // print <| Parse (Ok [Other ['2']; Other['*']; Other['3']; Other['+']; Other['4']])
    //print <| Parse (Ok [Other ['m';'o';'d']; IntToken 4; IntToken 2])
    //let a = ExtractRightAppList [] (Funcapp(Var ['f'], Funcapp(Var ['x'],Var ['y'])))
   // let b = List.rev a
    //print <| MakeLeftAppList b
    //print <| BuildAddExp (Ok [Other ['f']; Other['x']; Other ['y']]) []
    //print <| BuildAddExp (Ok [Other ['f']; Bracket['(']; Other['x']; Other ['y']; Bracket[')']]) []
    //print <| Parse (Ok([Keyword LET; Other ['f']; Other['x']; Keyword EQUAL; Other['x']; Other['+']; IntToken 1; Keyword IN; Keyword LET; Other ['g']; Other['y']; Keyword EQUAL; Other['y']; Other['+']; IntToken 2;Keyword IN; Other ['g']; Other['f'];IntToken 3] ) )
    //problem when putting two arguments
    print <| parse (Ok([Keyword LET; Other ['f']; Other['a']; Other['b']; Keyword EQUAL; Other['a']; Other['+']; Other ['b'];Other['/']; Other['a']; Keyword IN; Other ['f']; IntToken 3; IntToken 4] ) )
    //print <| Parse (Ok [Other ['f']; IntToken 3; IntToken 4; Other ['*']; Other ['f'];Other['+'];Other['b']]) 
    print <| parse (Ok [Other ['f']; Bracket['(']; Other['x']; Other ['y']; Bracket[')']]) 
    //print <| buildAddExp [] (Ok [Other['x']; Other ['y']]) 
    //print <| buildAddExp [] (Ok [Other ['f']; Bracket[')']])
    //print <| Parse (Ok([Keyword LET; Other ['f']; Other['x']; Other['y']; Keyword EQUAL; Other['x']; Keyword IN; Other ['f']; IntToken 3; IntToken 4] ) )
    0 // return an integer exit code
    