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
