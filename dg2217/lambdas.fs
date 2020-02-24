module Lambdas

type MathType = |Add|Sub|Mult|Div|Mod

type BuiltInType = 
    | Math of MathType
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

let concat s1 s2 =
    match (s1,s2) with
    | (Literal(String(a)),Literal(String(b))) -> Literal(String(a@b))

let rec implode lst = 
    match lst with
    | Null -> Literal(String([]))
    | Pair(Literal(String([c])),p) -> (Literal(String [c]), (implode p)) ||> concat

let rec explode str =
    match str with
    | [] -> Null
    | hd::tl -> Pair(Literal(String([hd])), explode tl)

concat (Literal(String['a';'b'])) (Literal(String ['c';'d']))
explode ['a';'b';'c';'d']
implode (Pair(Literal (String ['a']),Pair(Literal (String ['b']),Pair (Literal (String ['c']),Pair (Literal (String ['d']),Null)))))


let execArithmetic func A B =
    match func with
    //| P -> Pair()
    | Add -> A+B |> Int |> Literal //Literal(Int(valueA+valueB))
    | Sub -> A-B |> Int |> Literal
    | Mult -> A*B |> Int |> Literal
    | Div -> A/B |> Int |> Literal
    | Mod -> A%B |> Int |> Literal
        //| x -> BuiltInFunc(x)
    //| (a,b)->Funcapp(a,b)

let execPairOperation func (a,b) =
    match func with
    | PFst -> a 
    | PSnd -> b

let printpipe (x:AST) = printfn "%A" x;x

let rec exec (exp:AST) :AST =
    let orderExec o E =
        match o with
        | true -> exec E
        | false -> E 

    let rec betaReduce env exp = 
        let (|BASICS|_|) exp = 
            match exp with
            | FuncDefExp(name,body,E) -> 
                Some(betaReduce ((name,body)::env) (orderExec false E))
            | Funcapp(Lambda l,E) -> 
                Some(betaReduce ((l.InputVar,E)::env) (orderExec false l.Body))
            | Var(name) -> Some(findValue env name)    
            | _ -> None      

        match exp with
        | BASICS result -> result
        | Funcapp(ast1,ast2) ->
            match Funcapp(betaReduce env ast1, betaReduce env ast2) with
            | BASICS result -> result
            | x -> x
        | Lambda l -> Lambda {InputVar = l.InputVar; Body = betaReduce env l.Body}
        | x -> x 

    let (|MATCHEQUAL|_|) exp = 
        match exp with 
        | (Funcapp(Funcapp(BuiltInFunc(Equal),x),y)) -> Some(x,y) 
        | _->None

    let (|MATCHMATH|_|) exp = 
        match exp with 
        | (Funcapp(Funcapp(BuiltInFunc(Math(_)),x),y)) -> Some(x,y) 
        | _-> None

    let (|MATCH2ARG|_|) exp = 
        match exp with
        | Funcapp(Funcapp(BuiltInFunc(func),x),y) -> Some (func, x, y)
        | _ -> None

    let (|MATCH2ARG|_|) exp = 
        match exp with
        | Funcapp(BuiltInFunc(func),x) -> Some (func, x, y)
        | _ -> None

    let rec evaluate exp =
        let (|BASEFUNCS|_|) exp = 
            match exp with 
            | MATCH2ARG (Equal,x,y) -> Null//execEqual x y 
            | MATCH2ARG (Math op,x,y) -> Null //execFST x y
            | MATCH2ARG (P,x,y)-> Null
            | MATCHMATH (x,y) -> Null //execMath x y
            | 
            | Funcapp(Funcapp(BuiltInFunc(Equal),x),y) -> 
                if x=y then Some(trueAST) else Some(falseAST)
            | Funcapp(BuiltInFunc(func),Pair(a,b)) -> 
                (a,b) |> execPairOperation func |> Some
            | Funcapp(Funcapp(BuiltInFunc(Math func),Literal(Int a)),Literal(Int b)) -> 
                (a,b) ||> execArithmetic func |> Some// Some ((Literal (Int a),Literal (Int b)) ||> execArithmetic func)

            | Funcapp(BuiltInFunc(Implode), Literal(String s)) -> Some(explode s)
            | Funcapp(BuiltInFunc(Explode), Pair (a,b)) -> Pair(a,b) |> implode |> Some
            | Funcapp(BuiltInFunc(IsPair),x) -> 
                match x with | Pair(_) -> Some(trueAST) | _ -> Some(falseAST)
            | Funcapp(Literal(_), _) -> Some (printf "Cannot apply function to literal")
            | _ -> None  

        //printpipe exp
        match exp with
        | BASEFUNCS result -> result
        | ERRORFUNCS result -> result
        | Funcapp(a,b) -> 
            match Funcapp(evaluate a,evaluate b) with 
            | BASEFUNCS result -> result
            | x -> x
        | Lambda l -> Lambda {InputVar = l.InputVar; Body = evaluate l.Body} //|> evaluate//|> reevaluate env (Lambda l)        
        | x->x

    exp |> betaReduce [] |> evaluate

//let f a b = a+b/a in f 3.0 4.0
let test0 = Funcapp (Funcapp (BuiltInFunc Add,Literal (Int 2)),Literal (Int 1))
let test1 = Funcapp(Lambda{InputVar = ['x']; Body = Funcapp(Funcapp(BuiltInFunc(Add),Var(['x'])),Literal(Int(1)))},Literal(Int(9)))
let simpleMod = (Funcapp(Funcapp(BuiltInFunc(Mod),Literal(Int(120))),Literal(Int(100))))
let simpleDiv = (Funcapp(Funcapp(BuiltInFunc(Div),simpleMod),Literal(Int(2))))
let test2 = Funcapp(Funcapp(BuiltInFunc(Add),simpleDiv),Literal(Int(3)))


let division = (Funcapp(Funcapp(BuiltInFunc(Div),Var(['b'])),Var(['a'])))
let addition = (Funcapp(Funcapp(BuiltInFunc(Add),Var(['a'])),division))
let ABbody = Lambda {InputVar = ['a']; Body = Lambda{InputVar = ['b'];Body = addition}}
let FBody = Funcapp(Funcapp(Var(['f']),Literal(Int(3))),Literal(Int(27)))

let test3 = Funcapp(Funcapp(ABbody,Literal(Int(3))),Literal(Int(27)))

let test4 = FuncDefExp(['f'],ABbody,FBody) 
// let f a b = a + b/a in f 3 27

let test5FBody = Funcapp(Funcapp(BuiltInFunc Mult, Funcapp(Funcapp(BuiltInFunc Mult, Var ['f']),Var['f'])),Var ['f'])
let test5 = FuncDefExp(['f'],Literal(Int(5)),test5FBody)
// let f = 2 in f*f*f

let test6FBody = Funcapp(Funcapp(Var(['f']),Literal(Int(3))),Funcapp(Funcapp(BuiltInFunc(Mult),Literal(Int(9))),Literal(Int(9))))
let test6 = FuncDefExp(['f'],ABbody,test6FBody) 
// let f a b = a + b/a in f 3 9*9

let test7GBody = Lambda{InputVar = ['x'];Body = Funcapp(Var['f'],Var ['x'])}
let test7FBody =Lambda{InputVar = ['a'];Body = Lambda{InputVar = ['b']; Body = Funcapp(Funcapp(BuiltInFunc Add,Funcapp(Funcapp(BuiltInFunc Mult, Var ['a']),Var['a'])),Var['b'])}} 
let test7 = FuncDefExp(['f'],test7FBody,FuncDefExp(['g'],test7GBody,(Funcapp(Var ['f'],Literal(Int 3)))))   
// let g a b = a*a + b in let f x = g x in f 3 

let test8 = FuncDefExp(['f'],Lambda{InputVar = ['x'];Body = Funcapp(Funcapp(BuiltInFunc Add, Var ['x']),Literal (Int 1))},FuncDefExp(['g'],Lambda{InputVar = ['y']; Body = Funcapp (Funcapp (BuiltInFunc Add, Var ['y']), Literal (Int 2))},(Funcapp(Var ['f'], Funcapp(Var['g'], Literal(Int 3))))))   
//let f x = x+1 in let g y = y+2 in g (f 3)

let test9 = FuncDefExp(['f'],Lambda{InputVar = ['x']; Body = Funcapp(Funcapp(BuiltInFunc Add, Var ['x']),Literal (Int 1))},Funcapp (Var['f'],(Funcapp (Var ['f'], Literal(Int 3)))))  
// let f x = x+1 in f (f 3)

let result0 = exec test0
let result1 = exec test1
let result2 = exec test2
let result3 = exec test3
let result4 = exec test4
let result5 = exec test5
let result6 = exec test6
let result7 = exec test7
let result8 = exec test8
let result9 = exec test9

//let g a b = a*b*b in let f x y = (g x y)/2 in f 2 5

//let g a b = a+b 
//let f x = g x
//f 1

let f x = x+1
f (f 3)
