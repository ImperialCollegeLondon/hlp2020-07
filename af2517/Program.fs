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
    //print <| parse (Ok([Keyword "let"; Other "f"; Other "a"; Other "b"; Keyword "="; Other "a"; Other "+"; Other "b";Other "/"; Other "a"; Keyword "in"; Other "f"; IntegerLit 3; IntegerLit 4] ) )
    //print <| Parse (Ok [Other ['f']; IntToken 3; IntToken 4; Other ['*']; Other ['f'];Other['+'];Other['b']]) 
    //print <| parse (Ok [Other "f"; OpenRoundBracket; Other "x"; AddToken;Other "g"; CloseRoundBracket;]) 
    //print <| parse (Ok [Other "f"; OpenRoundBracket; Other "x"; AddToken; IntegerLit 1; CloseRoundBracket])
    //print <| parse (Ok [Let; Other "f"; Other"a"; EqualToken; Other "a"; Keyword "in"; Let; Other "g"; Other "b"; EqualToken; Other"b"; Keyword "in"; OpenRoundBracket; Other "f"; IntegerLit 3; CloseRoundBracket; MultToken; OpenRoundBracket; Other "g"; IntegerLit 2; CloseRoundBracket ])
    //let a = buildAddExp [] (Ok [Other "f"; OpenRoundBracket; Other "x"; Other "y"; Other "z";CloseRoundBracket])
    //let a = fst(buildAddExp [] (Ok [OpenRoundBracket; Other"f"; Other "g"; CloseRoundBracket; MultToken; OpenRoundBracket; Other "h"; Other "p"; CloseRoundBracket]))
    //print <| extractRightAppList [] a 

    //---------------- FIX THIS --------------------------//
    
    //print <| parse (Ok [OpenRoundBracket; Other"f"; Other "g"; CloseRoundBracket; MultToken; OpenRoundBracket; Other "h"; Other "p"; CloseRoundBracket])
    //let a = extractRightAppList [] (Funcapp(Funcapp(Var "x", Var "y"), Var "z"))
    //        |> List.rev
    //        |> makeLeftAppList
    //print a

    //let b = [Var "x"; Var "y"; Var "z"]
    //        |> List.rev
    //        |> makeLeftAppList
    //print b
    //print <| parse (Ok [OpenRoundBracket; Other"f"; Other "g"; CloseRoundBracket; MultToken; OpenRoundBracket; Other "h"; Other "p"; CloseRoundBracket])
    //print <| buildAddExp [] (Ok [Other "x"; Other "y"; Other "z"])
    //---------------------------------------------------//
    
    
    //print <| parse (Ok [Other"h";Keyword "if"; Other "x"; Keyword "then"; Other "y";  Keyword "else"; Other "z";  Keyword "fi"; Other"f"])
    
    
    
    
    //-------------------------FIX THIS -------------------------
    //print <| parse (Ok [Keyword "if"; Other "x"; AddToken; IntegerLit 1; Keyword "then"; Other "y";  AddToken; IntegerLit 1; Keyword "else"; Other "z";  AddToken; IntegerLit 1; Keyword "fi"])
    //print <| parse (Ok [OpenSquareBracket; IntegerLit 1; Keyword ";"; IntegerLit 2; Keyword ";"; IntegerLit 3; CloseSquareBracket])
    //-----------------------------------------------------------

    print <| parse (Ok [Let; Other "x"; EqualToken; Other "x"; MultToken; IntegerLit 2; Keyword "in"; Other "x"; IntegerLit 5])
    // print <| parse (Ok [OpenRoundBracket; Keyword "fun"; Other "x"; EqualToken; Other "x"; AddToken; IntegerLit 1; CloseRoundBracket])
    //print <| extractRightAppList [] (fst(a))
    //print <| parse ((Ok [Other "f"; OpenRoundBracket; Other "x"; Other "y";Other "z"; CloseRoundBracket]))
   // print <| parse ((Ok [Other "f"; OpenRoundBracket; Other "a"; OpenRoundBracket; Other "f"; Other "d"; CloseRoundBracket; CloseRoundBracket]))

    //let a = [1+2]
    //print a 
    //let b = buildAddExp [] (Ok [Other "x"; Other "y"; Other "z"])
    //print <| extractRightAppList [] (fst(b))
    //print <| parse ((Ok [ Other "x"; Other "y";Other "z"]))
    //print <| buildAddExp [] (Ok [Other['x']; Other ['y']]) 
    //print <| buildAddExp [] (Ok [Other ['f']; Bracket[')']])
    //print <| parse (Ok([Let; Other "f"; Other "x"; Other"y"; EqualToken; Other "x"; Keyword "in"; Other "f"; IntegerLit 3; IntegerLit 4] ) )
    //print <| parse (Ok([Let; Other "g"; EqualToken; Let; Other "f"; Other "x"; IntegerLit 4] ) )
    0 // return an integer exit code
   