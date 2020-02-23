//module Lambdas 

type BuiltInType = 
    | Add
    | Sub 
    | Mult 
    | Div 
    | Mod 
    | Test
    | Equal //works for strings ints and nulls 
    | Explode 
    | Implode 
    | P //creates a pair 
    | PFst 
    | PSnd
    | IsPair
    | IfThenElse  

type AST = 
    | FuncDefExp of FuncDefExpType:char list*AST*AST 
    | Lambda of LambdaType
    | Var of char list //only valid in lambdas 
    | Funcapp of AST*AST
    | Pair of AST*AST 
    | Null 
    | Literal of LitType 
    | BuiltInFunc of BuiltInType

and LambdaType = {
    InputVar: char list
    Body: AST
}
and LitType = 
    | Int of int 
    | String of char list 
    | True of AST //make it a named function with Lambda("A",Lambda ("B",Var "A" ))
    | False of AST 

and EnvironmentType = list<(char list)*AST>

let trueAST = Lambda {InputVar = ['x']; Body = Lambda {InputVar = ['y'];Body = Var ['x']}}
let falseAST = Lambda {InputVar = ['x']; Body = Lambda {InputVar = ['y']; Body = Var ['y']}}

let rec findValue (env:EnvironmentType) name=
    match env with
    | (n,v)::_ when n=name -> v
    | _::tl -> findValue tl name
    | _ -> Var(name)

let removeEntry env name = 
    env |> List.filter (fun (n,_) -> n<>name)

let execArithmetic func valueA valueB = 
    match func with
    //| P -> Pair()
    | Add -> Literal(Int(valueA+valueB))
    | Sub -> Literal(Int(valueA-valueB))
    | Mult -> Literal(Int(valueA*valueB))
    | Div -> Literal(Int(valueA/valueB))
    | Mod -> Literal(Int(valueA%valueB))
    | Equal -> if valueA=valueB then trueAST else falseAST
    | P -> Pair(Literal(Int valueA),Literal(Int valueB))
    | x -> BuiltInFunc(x)

let execPairOperation func (a,b) =
    match func with
    | PFst -> a 
    | PSnd -> b

let printpipe x = printfn "%A" x;x

let rec exec (env:EnvironmentType) (exp:AST) :AST =
    let reevaluate env exp1 exp2 =
        if exp1=exp2 then
            exp1
        else exec env exp2
    let orderExec o env E =
        match o with
        | true -> exec env E
        | false -> E 
    //printpipe env
    //printpipe exp
    match exp with
    | FuncDefExp(name,body,E) -> exec ((name,body)::env) (orderExec false env E)
    | Funcapp(Lambda l,E) -> exec ((l.InputVar,E)::env) (orderExec false env l.Body)
    | Var(name) -> findValue env name  
    | Funcapp(Funcapp(BuiltInFunc(Equal),x),y) -> if x=y then trueAST else falseAST
    | Funcapp(Funcapp(BuiltInFunc(func),Literal(Int x )),Literal(Int y)) -> execArithmetic func x y
    | Funcapp(BuiltInFunc(func),Pair(a,b)) -> execPairOperation func (a,b)
    | Funcapp(BuiltInFunc(IsPair),x) -> match x with | Pair(_) -> trueAST | _ -> falseAST 
    | Funcapp(ast1,ast2) -> Funcapp(exec env ast1, exec env ast2) |> reevaluate env (Funcapp(ast1,ast2))
    | Lambda l -> Lambda {InputVar = l.InputVar; Body = exec env l.Body} |> reevaluate env (Lambda l)
    | x -> x
    
//let f a b = a+b/a in f 3.0 4.0
let simple0 = Funcapp (Funcapp (BuiltInFunc Add,Literal (Int 2)),Literal (Int 1))
let simple = Funcapp(Lambda{InputVar = ['x']; Body = Funcapp(Funcapp(BuiltInFunc(Add),Var(['x'])),Literal(Int(1)))},Literal(Int(9)))
let simpleMod = (Funcapp(Funcapp(BuiltInFunc(Mod),Literal(Int(120))),Literal(Int(100))))
let simpleDiv = (Funcapp(Funcapp(BuiltInFunc(Div),simpleMod),Literal(Int(2))))
let ABbody2 = Funcapp(Funcapp(BuiltInFunc(Add),simpleDiv),Literal(Int(3)))


let division = (Funcapp(Funcapp(BuiltInFunc(Div),Var(['b'])),Var(['a'])))
let addition = (Funcapp(Funcapp(BuiltInFunc(Add),Var(['a'])),division))
let ABbody = Lambda {InputVar = ['a']; Body = Lambda{InputVar = ['b'];Body = addition}}
let FBody = Funcapp(Funcapp(Var(['f']),Literal(Int(3))),Literal(Int(27)))

let fig1easy = Funcapp(Funcapp(ABbody,Literal(Int(3))),Literal(Int(27)))

let fig1Test = FuncDefExp(['f'],ABbody,FBody) 
// let f a b = a + b/a in f 3 27

let cubeTest = FuncDefExp(['f'],Literal(Int(2)),FBody)
// let f = 2 in f*f*f

let FBodyOrder = Funcapp(Funcapp(Var(['f']),Literal(Int(3))),Funcapp(Funcapp(BuiltInFunc(Mult),Literal(Int(9))),Literal(Int(9))))
let orderTest = FuncDefExp(['f'],ABbody,FBodyOrder) 
// let f a b = a + b/a in f 3 9*9

//let closureTest = 
// let g a b = a + b*a in let f x = g x x in f 3 

let albertTest = FuncDefExp(['f'],Lambda{InputVar = ['x'];Body = Funcapp(Funcapp(BuiltInFunc Add, Var ['x']),Literal (Int 1))},FuncDefExp(['g'],Lambda{InputVar = ['y']; Body = Funcapp (Funcapp (BuiltInFunc Add, Var ['y']), Literal (Int 2))},(Funcapp(Var ['f'], Funcapp(Var['g'], Literal(Int 3))))))   
//let f x = x+1 in let g y = y+2 in g (f 3)

let albertEasy = FuncDefExp(['f'],Lambda{InputVar = ['x']; Body = Funcapp(Funcapp(BuiltInFunc Add, Var ['x']),Literal (Int 1))},Funcapp (Var['f'],(Funcapp (Var ['f'], Literal(Int 3)))))  
// let f x = x+1 in f (f 3)

let result0 = exec [] simple0
let result1 = exec [] simple
let result2 = exec [] ABbody2
let result3 = exec [] fig1easy
let result4 = exec [] fig1Test
let result5 = exec [] orderTest
let result6 = exec [] albertEasy
let result7 = exec [] albertTest

//let g a b = a*b*b in let f x y = (g x y)/2 in f 2 5

//let g a b = a+b 
//let f x = g x
//f 1

let f x = x+1
f (f 3)


