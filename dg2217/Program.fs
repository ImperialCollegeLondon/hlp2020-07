// Learn more about F# at http://fsharp.org

open System

open Lambdas

let print x =
    printfn "%A" x

let qe = exec


[<EntryPoint>]
let main argv =
    
    print <| exec (Var ['x'])
    print <| exec (Literal  <| Int 21)
    print <| exec (FuncDefExp
                    { Name = ['f']
                      Body =
                            Lambda
                              { InputVar = ['x']
                                Body =
                                      Funcapp
                                        (Funcapp (BuiltInFunc (Math Mult),Var ['x']),
                                         Literal (Int 2)) }
                      Expression = Funcapp (Var ['f'],Literal (Int 2)) })
    print <| exec (FuncDefExp
                  { Name = ['f']
                    Body = Lambda { InputVar = ['x']
                                    Body = Var ['x'] }
                    Expression =
                                FuncDefExp
                                  { Name = ['g']
                                    Body = Lambda { InputVar = ['x']
                                                    Body = Var ['x'] }
                                    Expression =
                                                Funcapp
                                                  (Funcapp (Var ['g'],Var ['f']),
                                                   Literal (Int 21)) } })
    print <| "Here A"
    print <| exec( (FuncDefExp
                   { Name = ['f']
                     Body =
                           Lambda
                             { InputVar = ['x']
                               Body =
                                     Lambda
                                       { InputVar = ['y']
                                         Body =
                                               Funcapp
                                                 (Funcapp
                                                    (BuiltInFunc (Math Add),
                                                     Funcapp
                                                       (Funcapp
                                                          (BuiltInFunc (Math Mult),Var ['x']),
                                                        Funcapp
                                                          (Funcapp
                                                             (BuiltInFunc (Math Mult),
                                                              Var ['y']),Literal (Int 2)))),
                                                  Literal (Int 1)) } }
                     Expression =
                                 FuncDefExp
                                   { Name = ['g']
                                     Body = Lambda { InputVar = ['y']
                                                     Body = Var ['y'] }
                                     Expression =
                                                 Funcapp
                                                   (Funcapp (Var ['f'],Literal (Int 2)),
                                                    Funcapp (Var ['g'],Literal (Int 5))) } }))
    
    
    (*
    (FuncDefExp
   { Name = ['f']
     Body =
           Lambda
             { InputVar = "x"
               Body =
                     Funcapp
                       (Funcapp (BuiltInFunc (Math Mult),Var "x"),
                        Literal (Int 2)) }
     Expression = Funcapp (Var "f",Literal (Int 2)) }, Ok [])

    *)
    0 // return an integer exit code
