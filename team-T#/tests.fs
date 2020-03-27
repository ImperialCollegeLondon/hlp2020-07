module TestsModule
open Definitions
open ParserModule
open LambdasRuntimeModule
open TopModule    

let parserTestDescriptions = 
    let reformatTest (name, instr, outI) =
            let tokLst = instr
            match (tokLst |> parsedOutput |> fst) with 
            | Ok ast -> (name,(Ok ast), outI, (sprintf "%A" tokLst))
            | Error msg -> (name, (Error msg), outI, (sprintf "%A" tokLst))
    [
        "first test",(Ok [Other "x"; Other "y"]),(Ok(FuncApp(Var ['x'], Var ['y'])))
        "second test",(Ok [Keyword "if"; Other "x";  IntegerLit 1L; Keyword "then"; Other "y"; IntegerLit 1L; Keyword "else"; Other "z"; IntegerLit 1L; Keyword "fi"]) ,(Ok(FuncApp(FuncApp(FuncApp (Var ['x'],Literal (Int 1L)),Lzy (FuncApp (Var ['y'],Literal (Int 1L)))),Lzy (FuncApp (Var ['z'],Literal (Int 1L))))))
        "third test", (Ok [Let; Other "x"; EqualToken; Other "x"; MultToken; IntegerLit 2L; Other "in"; Other "x"; IntegerLit 5L]), (Ok(FuncDefExp{ Name = ['x'];Body = FuncApp (FuncApp (BFunc (Mat Mult),Var ['x']),Literal (Int 2L));Expression = FuncApp (Var ['x'],Literal (Int 5L)) }))   
        "fourth test", (Ok [OpenSquareBracket; CloseSquareBracket; Other "x"; Other"y"]), Ok (FuncApp (FuncApp (Null,Var ['x']),Var ['y']))
        //"fifth test", (Ok [Let; Other "f"; Other "x"; EqualToken; Other "x"; MultToken; IntegerLit 2L]), (Error "This function definition is never used")
        "sixth test", (Ok [Let; Other "x"; EqualToken; Other "x"; MultToken; IntegerLit 2L; Other "in"; Other "x"; IntegerLit 5L]), (Ok(FuncDefExp{ Name = ['x'];Body = FuncApp (FuncApp (BFunc (Mat Mult),Var ['x']),Literal (Int 2L));Expression = FuncApp (Var ['x'],Literal (Int 5L)) }))
        "seventh test", (Ok [OpenRoundBracket; Other "fun"; Other "x"; EqualToken; Other "x"; AddToken; IntegerLit 1L; CloseRoundBracket]), (Ok(Lambda{ InputVar = ['x'];Body = FuncApp (FuncApp (BFunc (Mat Add),Var ['x']),Literal (Int 1L)) }))
        "eigth test", (Ok [Other "f"; OpenRoundBracket; Other "x"; Other "y";Other "z"; CloseRoundBracket]), Ok (FuncApp (Var ['f'],FuncApp (FuncApp (Var ['x'],Var ['y']),Var ['z'])))
        "ninth test", Ok [Other "f"; OpenRoundBracket; Other "a"; OpenRoundBracket; Other "f"; Other "d"; CloseRoundBracket; CloseRoundBracket], Ok (FuncApp (Var ['f'],FuncApp (Var ['a'],FuncApp (Var ['f'],Var ['d']))))
        "tenth test", Ok [OpenSquareBracket; OpenSquareBracket; Other "x"; CloseSquareBracket], Error "Input list is not valid"
        "eleventh test", Ok([Let; Other "f"; Other "x"; Other"y"; EqualToken; Other "x"; Other "in"; Other "f"; IntegerLit 3L; IntegerLit 4L]), Ok(FuncDefExp{ Name = ['f'];Body = Lambda { InputVar = ['x'];Body = Lambda { InputVar = ['y'];Body = Var ['x'] } };Expression =FuncApp(FuncApp (Var ['f'],Literal (Int 3L)),Literal (Int 4L)) })
        "test 12", Ok [OpenSquareBracket; Other "x";  IntegerLit 1L; Keyword ";"; Other "y"; IntegerLit 2L; Keyword ";"; Other "z"; IntegerLit 3L; CloseSquareBracket ], Ok(Pair(FuncApp (Var ['x'],Literal (Int 1L)),Pair(FuncApp (Var ['y'],Literal (Int 2L)),Pair (FuncApp (Var ['z'],Literal (Int 3L)),Null))))
        "test 13", Ok [OpenSquareBracket; Other "x"; AddToken; IntegerLit 1L; Keyword ";"; Other "y"; MultToken; IntegerLit 2L; CloseSquareBracket ], Ok(Pair(FuncApp (FuncApp (BFunc (Mat Add),Var ['x']),Literal (Int 1L)),Pair (FuncApp (FuncApp (BFunc (Mat Mult),Var ['y']),Literal (Int 2L)),Null)))
        "test 14", Ok [Other "f";MultToken; Other "g";MultToken; Other "h";AddToken; IntegerLit 1L], Ok(FuncApp(FuncApp(BFunc (Mat Add),FuncApp(FuncApp (BFunc (Mat Mult),Var ['f']),FuncApp (FuncApp (BFunc (Mat Mult),Var ['g']),Var ['h']))),Literal (Int 1L)))
        "test 15", Ok [Other"h";Keyword "if"; Other "x"; Keyword "then"; Other "y";  Keyword "else"; Other "z";  Keyword "fi"; Other"f"], Ok(FuncApp(FuncApp(Var ['h'],FuncApp (FuncApp (Var ['x'],Lzy (Var ['y'])),Lzy (Var ['z']))),Var ['f']))
        "test 16", Ok [Let; Other "f"; Other "x"; Other "y"; EqualToken; OpenSquareBracket; Other "x";Keyword ";"; Other "x"; MultToken; Other "x"; Keyword ";"; Other "x"; MultToken;Other "x"; MultToken; Other "x"; Keyword ";"; OpenSquareBracket; Other "x";AddToken; Other "y"; CloseSquareBracket; CloseSquareBracket; Other "in";Other "f"; IntegerLit 3L; IntegerLit 7L], (Ok
        (FuncDefExp
           { Name = ['f']
             Body =
                   Lambda
                     { InputVar = ['x']
                       Body =
                             Lambda
                               { InputVar = ['y']
                                 Body =
                                       Pair
                                         (Var ['x'],
                                          Pair
                                            (FuncApp
                                               (FuncApp (BFunc (Mat Mult),Var ['x']),
                                                Var ['x']),
                                             Pair
                                               (FuncApp
                                                  (FuncApp
                                                     (BFunc (Mat Mult),Var ['x']),
                                                   FuncApp
                                                     (FuncApp
                                                        (BFunc (Mat Mult),Var ['x']),
                                                      Var ['x'])),
                                                Pair
                                                  (Pair
                                                     (FuncApp
                                                        (FuncApp
                                                           (BFunc (Mat Add),Var ['x']),
                                                         Var ['y']),Null),Null)))) } }
             Expression =
                         FuncApp (FuncApp (Var ['f'],Literal (Int 3L)),Literal (Int 7L)) }))
    ]  
    |> List.map reformatTest


let test0 = FuncApp (FuncApp (Literal(Int 3L),Literal (Int 2L)),Literal (Int 1L))
let test1 = FuncApp(Lambda{InputVar = ['x']; Body = FuncApp(FuncApp(BFunc(Mat Add),Var(['x'])),Literal(Int(1L)))},Literal(Int(9L)))

let simpleMod = (FuncApp(FuncApp(BFunc(Mat Mod),Literal(Int(120L))),Literal(Int(0L))))
let simpleDiv = (FuncApp(FuncApp(BFunc(Mat Div),simpleMod),Literal(Int(2L))))
let test2 = FuncApp(FuncApp(BFunc(Mat Add),simpleDiv),Literal(Int(3L)))

let div3 = (FuncApp(FuncApp(BFunc(Mat Div),Var(['b'])),Var(['a'])))
let add3 = (FuncApp(FuncApp(BFunc(Mat Add),Var(['a'])),div3))
let ABbody = Lambda {InputVar = ['a']; Body = Lambda{InputVar = ['b'];Body = add3}}
let test3 = FuncApp(FuncApp(ABbody,Literal(Int(3L))),Literal(Int(27L)))

let div4 = (FuncApp(FuncApp(BFunc(Mat Div),Var(['n'])),Var(['a'])))
let add4 = (FuncApp(FuncApp(BFunc(Mat Add),Var(['a'])),div4))
let ABbody4 = Lambda {InputVar = ['a']; Body = Lambda{InputVar = ['b'];Body = add4}}
let FBody = FuncApp(FuncApp(Var(['f']),Literal(Int(4L))),Literal(Int(36L)))
let test4 = FuncDefExp{Name = ['f'];Body = ABbody4;Expression = FBody}
// let f a b = a + b/a in f 3 27

let test5FBody = FuncApp(FuncApp(BFunc (Mat Mult), FuncApp(FuncApp(BFunc (Mat Mult), Var ['f']),Var['f'])),Var ['f'])
let test5 = FuncDefExp{Name = ['f'];Body = Literal(Int(5L)); Expression = test5FBody}
// let f = 5 in f*f*f

let test6FBody = FuncApp(FuncApp(Var(['f']),Literal(Int(3L))),FuncApp(FuncApp(BFunc(Implode),Literal(Int(9L))),Literal(Int(9L))))
let test6 = FuncDefExp{Name = ['f']; Body = ABbody; Expression =  test6FBody} 
// let f a b = a + b/a in f 3 (9*9)

let test7GBody = Lambda{InputVar = ['x'];Body = FuncApp(Var['f'],Var ['x'])}
let test7FBody =Lambda{InputVar = ['a'];Body = Lambda{InputVar = ['b']; Body = FuncApp(FuncApp(BFunc (Mat Add),FuncApp(FuncApp(BFunc (Mat Mult), Var ['a']),Var['a'])),Var['b'])}} 
let test7 = FuncDefExp{Name = ['f'];Body = test7FBody;Expression = FuncDefExp{Name = ['g'];Body = test7GBody;Expression = (FuncApp(Var ['g'],Literal(Int 3L)))}}  
// let f a b = a*a + b in let g x = f x in g 3 

let test8 = FuncDefExp{Name = ['f']; Body = Lambda{InputVar = ['x'];Body = FuncApp(FuncApp(BFunc (Mat Add), Var ['x']),Literal (Int 1L))};Expression = FuncDefExp{Name = ['g'];Body = Lambda{InputVar = ['y']; Body = FuncApp (FuncApp (BFunc (Mat Add), Var ['y']), Literal (Int 2L))};Expression = (FuncApp(Var ['f'], FuncApp(Var['g'], Literal(Int 3L))))}} 
//let f x = x+1 in let g y = y+2 in g (f 3)

let test9 = FuncDefExp{Name = ['f'];Body = Lambda{InputVar = ['x']; Body = FuncApp(FuncApp(BFunc (Mat Add), Var ['x']),Literal (Int 1L))};Expression = FuncApp (Var['f'],(FuncApp (Var ['f'], Literal(Str ['a']))))}  
// let f x = x+1 in f (f "a")

let test10 = FuncApp(FuncApp(FuncApp(FuncApp(BFunc(Equal),Literal(Int 2L)),Literal(Int 1L)),Lzy(test8)),Lzy(test9))
// if 2=1 then test9 else test8
let test11 = FuncApp(FuncApp(FuncApp(FuncApp(BFunc(Equal),Literal(Int 2L)),Literal(Int 2L)),Lzy(test8)),Lzy(test9))
// if 2=2 then test9 else test8

let elseBody12 = FuncApp(FuncApp(BFunc(Mat Mult),Var ['n']),FuncApp(Var ['f'], FuncApp(FuncApp(BFunc(Mat Sub),Var ['n']),Literal(Int 1L))))
let ifStatement12 = FuncApp(FuncApp(FuncApp(FuncApp(BFunc Equal,Var ['n']),Literal(Int 0L)),Lzy(Literal(Int 1L))),Lzy(elseBody12))
let fBody12 = Lambda{InputVar = ['f']; Body = Lambda{InputVar = ['n']; Body = ifStatement12}}
let test12 = FuncDefExp{Name = ['f'];Body = fBody12;Expression = FuncApp(FuncApp(Var ['f'],Lzy(FuncApp(Y,Var['f']))),Literal(Int 5L))}
// let rec f n = if n = 0 then 1 else n*f(n-1) in f 2 SAME AS let f' f n = if n = 0 then 1 else n*f(n-1) in f' (Y h) 2 

let fminus1 = FuncApp(Var ['f'], FuncApp(FuncApp(BFunc (Mat Sub), Var ['a']),Literal (Int 1L)))
let fminus2 = FuncApp(Var ['f'], FuncApp(FuncApp(BFunc (Mat Sub), Var ['a']),Literal (Int 2L)))
let recBody = FuncApp(FuncApp(BFunc (Mat Add), fminus1),fminus2)
let eqBody1 = FuncApp(FuncApp(BFunc Equal, Var ['a']), Literal(Int 1L))
let eqBody0 = FuncApp(FuncApp(BFunc Equal, Var ['a']), Literal(Int 0L))
let ifelseBody13 = FuncApp(FuncApp(eqBody0,Lzy(Literal(Int 0L))),Lzy(FuncApp(FuncApp(eqBody1,Lzy(Literal(Int 1L))),Lzy(recBody)))) 
let test13 = 
    FuncDefExp{
        Name = ['f'];
        Body = Lambda{InputVar = ['f']; Body = Lambda {InputVar = ['a']; Body = ifelseBody13}};
        Expression = FuncApp(FuncApp(Var ['f'],Lzy(FuncApp(Y,Var['f']))),Literal(Int 25L))} 
//fibonacci
//let rec f a = if a = 0 then 0 else if a = 1 then 1 else f(a-1) + f(a-2) in f 92

let tailBody14 = FuncApp(Var ['f'], FuncApp(BFunc (PSnd), Var ['p']))
let headBody14 = FuncApp(FuncApp(BFunc (Mat Mult),FuncApp(BFunc PFst,(Var['p']))),Literal (Int 2L))
let elseBody14 = FuncApp(FuncApp(BFunc P, Lzy(headBody14)),Lzy(tailBody14))
let eqBody14 = FuncApp(FuncApp(BFunc Equal, Var ['p']), Null)
let ifelseBody14 = FuncApp(FuncApp(eqBody14,Lzy(Null)),Lzy(elseBody14)) 
let list14 = Pair(Literal(Int 1L), Pair(FuncApp(FuncApp(BFunc(Mat Mult), Literal(Int 3L)),Literal(Int 2L)), Pair(Literal(Int 2L), Pair(test12, Null))))
let test14 =
    FuncDefExp{
        Name = ['f'];
        Body = Lambda{InputVar = ['f']; Body = Lambda {InputVar = ['p']; Body = ifelseBody14}};
        Expression = FuncApp(FuncApp(Var ['f'],Lzy(FuncApp(Y,Var['f']))),list14)}  
//let rec f p = if p = [] then [] else  ((p.Head)*2)::f(p.Tail) in f [1;2*3;3;test12]

let test_List_Lambda_Runtime =
    [
        (   
            "Runtime Test 0",
            exec test0,
            Error "Run time error: FuncApp (Literal (Int 3L),Literal (Int 2L)) is not a valid function application",
            "3(2(1))"
        )
        (
            "Runtime Test 1",
            exec test1,
            (Ok(Literal (Int 10L))),
            "(fun x -> x+1) 9"
        )        
        (
            "Runtime Test 2",
            exec test2,
            Error "Run-time error: Cannot divide by 0!",
            "(120%0)/2+3"        
        )
        (
            "Runtime Test 3",
             (exec test3),
             (Ok(Literal (Int 12L))),
             "((fun a -> fun b -> a + b/a)3)27"        
        )
        (
            "Runtime Test 4",
            (exec test4), 
            (Error """Run-time error: "n" is not defined"""),
            "f a b = a+n/b in f 4 36"
        )       
        (
            "Runtime Test 5",
            (exec test5), 
            (Ok(Literal (Int 125L))),
            "let f = 5 in f*f*f"        
        )
        (
            "Runtime Test 6",
            (exec test6),
            (Error "Run-time error: Wrong argument given to implode"),
            "let f a b = a + b/a in f 3 (Implode 9 9)"
        )
        (
            "Runtime Test 7",
            (exec test7),
            (Ok(Lambda { InputVar = ['b']; Body = FuncApp(FuncApp(BFunc (Mat Add),FuncApp(FuncApp (BFunc (Mat Mult),Literal (Int 3L)),Literal (Int 3L))),Var ['b'])})), 
            "let f a b = a*a + b in let g x = f x in g 3 "      
        )
        (
            "Runtime Test 8",
            (exec test8),
            (Ok(Literal (Int 6L))),
            "let f x = x+1 in let g y = y+2 in g (f 3)"        
        )
        (
            "Runtime Test 9",
            (exec test9),
            (Error "Run-time error: Add(Literal (Str ['a']),Literal (Int 1L)) , is not a valid expression"),
            """let f x = x+1 in f (f "a")"""        
        )
        (
            "Runtime Test 10",
            (exec test10),
            (Error "Run-time error: Add(Literal (Str ['a']),Literal (Int 1L)) , is not a valid expression"),
            "if 2=1 then test9 else test8"
        )
        (
            "Runtime Test 11",
            (exec test11),
            (Ok(Literal (Int 6L))),
            "if 2=2 then test9 else test8"        
        )
        (
            "Runtime Test 12",
            (exec test12),
            (Ok(Literal (Int 120L))),
            "FACTORIAL: let rec f n = if n = 0 then 1 else n*f(n-1) in f 5"        
        )
        (
            "Runtime Test 13, FIBONACI",
            (exec test13),
            (Ok(Literal (Int 75025L))),
            "FIBONACCI: let rec f a = if a = 0 then 0 else if a = 1 then 1 else f(a-1) + f(a-2) in f 25"        
        )
        (
            "Runtime Test 14, LIST RECURSIVE OPERATION",
            (exec test14),
            (Ok(Pair(Literal (Int 2L),Pair(Literal (Int 12L),Pair (Literal (Int 4L),Pair (Literal (Int 240L),Null)))))), 
            "LISTS & RECURSION: let rec f p = if p = [] then [] else  ((p.Head)*2)::f(p.Tail) in f [1;2*3;3;let rec f n = if n = 0 then 1 else n*f(n-1) in f 5]"        
        )
    ]

let bindPairHelperDescriptions =
    [
        (
            "bindPairHelper 1",
            bindPairHelper ( Pair (Literal (Str ['x']), Null)  ) Null [],
            None,
            "when there is nothing to match it should not match"
        )
        (
            "bindPairHelper 2 - Pair",
            bindPairHelper ( Pair (Literal (Int 21L), Null)  ) (Pair (Var ['x'],Null)) [],
            Some [(Pair (Literal (Int 21L),Null), Var ['x'])],
            "one item in 'general' notation should return binded variable as list - one item"
        )
        (
            "bindPairHelper 2 - ExactPairMatch",
            bindPairHelper ( Pair (Literal (Int 21L), Null)  ) (ExactPairMatch (Var ['x'],Null)) [],
            Some [(Literal (Int 21L), Var ['x'])],
            "one item in 'specific' notation should return binded variable correct"
        )
        (
            "bindPairHelper 3 - Pair",
            bindPairHelper ( Pair (Literal (Int 21L), Pair ( Literal (Int 20L), Null  ) )  ) (Pair (Var ['x'],Null)) [],
            Some [(Pair (Literal (Int 21L),Pair (Literal (Int 20L),Null)), Var ['x'])],
            "one item in 'general' notation should return binded variable as list - two items"
        )
        (
            "bindPairHelper 3 - ExactPairMatch",
            bindPairHelper ( Pair (Literal (Int 21L), Pair ( Literal (Int 20L), Null  ) )  ) (ExactPairMatch (Var ['x'],Null)) [],
            None ,
            "no specific match as lists of different sizes"
        )
        (
            "bindPairHelper 4 - Pair",
            bindPairHelper ( Pair (Literal (Int 21L), Pair ( Literal (Int 20L), Null  ) )  ) (Pair (Var ['x'], Pair (Var ['y';'z'],Null)  )) [],
            Some [(Literal (Int 21L), Var ['x']);(Pair (Literal (Int 20L),Null), Var ['y'; 'z'])],
            "First item should be 'specifically' binded (aka the head) and the other variable should match the tail"
        )
        (
            "bindPairHelper 4 - ExactPairMatch",
            bindPairHelper ( Pair (Literal (Int 21L), Pair ( Literal (Int 20L), Null  ) )  ) (ExactPairMatch (Var ['x'], ExactPairMatch (Var ['y';'z'],Null)  )) [],
            Some [(Literal (Int 21L), Var ['x']); (Literal (Int 20L), Var ['y'; 'z'])],
            "Specific match"
        )
        (
            "bindPairHelper 5 - Pair",
            bindPairHelper ( Pair (Literal (Int 21L), Pair ( Literal (Int 20L), Pair (Literal (Int 101L),Null)  ) )  ) (Pair (Var ['x'], Pair (Var ['y';'z'],Null)  )) [],
            Some [(Literal (Int 21L), Var ['x']); (Pair (Literal (Int 20L),Pair (Literal (Int 101L),Null)), Var ['y'; 'z'])],
            "First item should be 'specifically' binded (aka the head) and the other variable should match the tail"
        )
        (
            "bindPairHelper 5 - ExactPairMatch",
            bindPairHelper ( Pair (Literal (Int 21L), Pair ( Literal (Int 20L), Pair (Literal (Int 101L),Null)  ) )  ) (ExactPairMatch (Var ['x'], ExactPairMatch (Var ['y';'z'],Null)  )) [],
            None,
            "Length differs so it should not match"
        )
        (
            "bindPairHelper 6 - ExactPairMatch",
            bindPairHelper ( Pair (Literal (Int 21L), Pair ( Literal (Int 20L), Null  ) )  ) (ExactPairMatch ( Literal (Int 21L), ExactPairMatch (Var ['y';'z'],Null)  )) [],
            Some [(Literal (Int 20L), Var ['y'; 'z'])],
            "Including literals in the cases part"
        )
       ]
        
let bindEmptyVariablesDescriptions =
    [
        (
            "bindEmptyVariables 1 Pairs",
            bindEmptyVariables Null,
            [],
            "bindEmptyVariables works for empty list"
        )
        (
            "bindEmptyVariables 2 Pairs",
            bindEmptyVariables (Pair (Var ['x'],Null)),
            [(['x'], Var ['x'])],
            "bindEmptyVariables works for one variable"
        )
        (
            "bindEmptyVariables 3 Pairs",
            bindEmptyVariables (Pair (Var ['x'], Pair ( Var ['y';'z'], Null))),
            [(['x'], Var ['x']);(['y';'z'], Var ['y';'z'])],
            "bindEmptyVariables works for two variables - variable name contains more than 1 character"
        )
        (
            "bindEmptyVariables 1 ExactPairMatch",
            bindEmptyVariables (ExactPairMatch (Var ['x'],Null)),
            [(['x'], Var ['x'])],
            "bindEmptyVariables works for one variable"
        )
        (
            "bindEmptyVariables 2 ExactPairMatch",
            bindEmptyVariables (ExactPairMatch (Var ['x'],Null)),
            [(['x'], Var ['x'])],
            "bindEmptyVariables works for one variable"
        )
        (
            "bindEmptyVariables 3 ExactPairMatch",
            bindEmptyVariables (ExactPairMatch (Var ['x'], ExactPairMatch ( Var ['y';'z'], Null))),
            [(['x'], Var ['x']);(['y';'z'], Var ['y';'z'])],
            "bindEmptyVariables works for two variables - variable name contains more than 1 character"
        )
    ]

let testMatchDescriptions =
    [
        (
         "Values Defined outside 1",
         lambdaEval "let f = [1;2;3;4;5] in let g = 2 in match f case [x;y;z;a;b;c;d] -> 1 + x + g case [x;y] -> 2 + g + x case [x] -> 3 case endmatch",
         Ok (Literal (Int 5L)),
         "Pair defined as value outside\nCase uses value defined outside\n"
        )
        (
         "Values Defined outside 2",
         lambdaEval "let f = [] in let g = 2 in match f case [x;y;z;a;b;c;d] -> 1 + x + g case [x;y] -> 2 + g + x case [] -> 101 case [x] -> 3  case endmatch",
         Ok (Literal (Int 101L)),
         "Empty list should be empty"
        )
        (
         "Values Defined outside 3",
         lambdaEval "let f = [1;2;3] in let g = 2 in let j = 21 in match f case [x;y;z;a;b;c;d] -> 1 + x + g case [x;y] -> j * g case [x] -> 3 case [] -> 101 case endmatch",
         Ok (Literal (Int 42L)),
         "Shadowing cases by having more generic ones before should work"
        )
        (
         "Values Defined outside 4",
         lambdaEval "let f = [1;2;3] in let g = 2 in let j = 21 in match f case [x;y;z] -> x + y case [x;y] -> j * g case [x] -> 3 case [] -> 101 case endmatch",
         Ok (Literal (Int 3L)),
         "Just enough variables for all but the last element"
        )
        (
         "Values Defined outside 5",
         lambdaEval "let f = 10 in let g = 2 in let j = 21 in match f * g * j case 419 -> j * j case 421 -> g * g * g * g case 420 -> f * f * f case endmatch",
         Ok (Literal (Int 1000L)),
         "Condition of match is itself a FuncApp"
        )
        (
         "Simple Match 1",
         lambdaEval "match 1 case 1 -> 1 case endmatch",
         Ok (Literal (Int 1L)),
         "Single case"
        )
        (
         "Simple Match 2",
         lambdaEval "match 1 case 2 -> 2 case 1 -> 1 case endmatch",
         Ok (Literal (Int 1L)),
         "Double case"
        )
        (
         "Nested Match return value 1",
         lambdaEval "match 1 case 1 -> match 2 case 2 -> match 3 case 3 -> 71 case endmatch case 3 -> 3 case endmatch case endmatch",
         Ok (Literal (Int 71L)),
         "Only values nested match"
        )
        (
         "Nested Match return value 2",
         lambdaEval "match 1 case 3 -> 49 case 1 ->  match [1;2;3;4] case [] -> 1 case [x;y] -> x + 5 case endmatch case endmatch",
         Ok (Literal (Int 6L)),
         "Values and \"lists\" nested match"
        )
        (
         "Nested Match return value 3",
         lambdaEval "match 1 case 3 -> 49 case 1 ->  match [1;2;3;4] case [] -> 1 case [x;y;z] -> x + y * 20 case endmatch case endmatch",
         Ok (Literal (Int 41L)),
         "Values and \"lists\" nested match"
        )
        (
         "Nested Match return pair 1",
         lambdaEval "match 1 case 3 -> 49 case 1 ->  match [1;2;3;4] case [] -> 1 case [x;y;z] -> z case endmatch case endmatch",
         lambdaEval "[3;4]",
         "Values and \"lists\" nested match"
        )
        (
         "Nested Match return pair 2",
         lambdaEval "match 1 case 3 -> 49 case 1 ->  match [1;2;3;4] case [] -> 1 case [x;y;z] -> [] case endmatch case endmatch",
         lambdaEval "[]",
         "Values and \"lists\" nested match"
        )
        (
         "Nested Match return pair 3",
         lambdaEval "match 1 case 3 -> 49 case 1 ->  match [] case [] -> 1 case [x;y;z] -> [] case endmatch case endmatch",
         lambdaEval "1",
         "Values and \"lists\" nested match"
        )
        (
         "Values outside nested match pairs 1",
         lambdaEval "let f = [] in let g = [1;2;3;4] in let x = 20 in match f case [] -> g case [x] -> x case [1;2;x] -> 3 case endmatch",
         lambdaEval "[1;2;3;4]",
         "Values and \"lists\" nested match"
        )
        (
         "Values outside nested match pairs 2",
         lambdaEval "let f = [] in let g = [1;2;3;4] in let x = 20 in match f case [] -> match g case [1;2;3;x] -> x case [] -> 3 case endmatch case [x] -> x case [1;2;x] -> 3 case endmatch",
         lambdaEval "[4]",
         "Values and \"lists\" nested match"
        )
        (
            "EDGE CASES 1",
            lambdaEval "let f = [] in let g = [1;2;3;4] in match g case [x;y;z;f;d] -> 100 case [] -> 3 case [x] -> x case endmatch",
            lambdaEval "100",
            "todo"
        )
        (
            "EDGE CASES 2",
            lambdaEval "let f = [] in let g = [1;2;3;4] in match g case [x;y;z;f;d;e] -> 0 case [] -> 1 case [x] -> 100 case endmatch",
            lambdaEval "100",
            "todo"
        )
        (
            "EDGE CASES 3",
            lambdaEval "let f = [] in let g = [1;2;3;4;5] in match g case [x;y] -> match x case 1 -> 1 case endmatch  case [x] -> 1 case endmatch",
            lambdaEval "1",
            "todo"
        )
        (
            "EDGE CASES 4",
            lambdaEval "let f = [] in let g = [1;2;3] in match g case {x:y:z} -> x + y + z case [x] -> 1 case endmatch",
            lambdaEval "6",
            "todo"
        )
        (
            "EDGE CASES 5",
            lambdaEval "let f = [] in let g = [1;2;3] in match g case {x:y} -> x + y case [x;y;z;f] -> x + y + z case endmatch",
            lambdaEval "6",
            "todo"
        )
        (
            "EDGE CASES 6",
            lambdaEval "let f = [] in let g = [1;2;3] in match g case {x:y} -> x + y case [x;y;z;d;f] -> 100 case {x} -> 101 case [x;y] -> 5 case endmatch",
            lambdaEval "5",
            "todo"
        )
        (
            "EDGE CASES 7",
            lambdaEval "let f = [] in let g = [1;2;3] in match g case {x:y} -> x + y case [x;y;z;d;f] -> 100 case {x} -> 101 case [x;y] -> match y case [x] -> 5 case endmatch case endmatch",
            lambdaEval "5",
            "todo"
        )
        (
            "EDGE CASES 8",
            lambdaEval "let f = [] in let g = [1;2;3] in match g case {x:y} -> x + y case [x;y;z;d;f] -> 100 case {x} -> 101 case [x;y] -> match y case {t:u} -> t + u + 3 case endmatch case endmatch",
            lambdaEval "8",
            "todo"
        )
        (
            "EDGE CASES 9",
            lambdaEval "let f = [] in let g = [1] in match g case [] -> 101 case [x;y] -> x case endmatch",
            lambdaEval "1",
            "todo"
        )
        //+ 100 case {} -> 150 case {x} -> x + 10 case
        (
            "EDGE CASES 10",
            lambdaEval "let f = [] in let g = [1] in match g case [] -> 101 case [x;y;z] -> 102 case {x:y} -> x + y case {} -> 150   case [x] -> 1 case endmatch",
            lambdaEval "1",
            "todo"
        )
        (
            "EDGE CASES 11",
            lambdaEval "let f = [] in let g = [1] in match g case [] -> 101 case [x;y;z] -> 102 case {x:y} -> x + y case {} -> 150 case {x} -> x + 10  case [x] -> 1 case endmatch",
            lambdaEval "11",
            "todo"
        )
        (
            "EDGE CASES 12",
            lambdaEval "let f = [] in let g = [1] in match g case [] -> 101 case [x;y;z] -> 102 case [x] -> match x case {l} -> l + 40 case [] -> 500 case endmatch case {x:y} -> x + y case {} -> 150 case {x} -> x + 10  case endmatch",
            lambdaEval "41",
            "todo"
        )
        (
            "EDGE CASES 13",
            lambdaEval "let f = [100] in match f case [x] -> match x case [y] -> match y case [] -> 400 case [z] -> match z case {l} -> l case endmatch case endmatch case endmatch case endmatch",
            lambdaEval "100",
            "todo"
        )
        (
            "EDGE CASES 14",
            lambdaEval "let f = [100] in match f case [x] -> match x case [y] -> match y case [] -> 400 case {c} -> c * 1000 case [z] -> match z case {l} -> l case endmatch case endmatch case endmatch case endmatch",
            lambdaEval "100000",
            "todo"
        )
        (
            "EDGE CASES 15",
            lambdaEval "let o = 0 in let q = [1;1;1;1;1] in match q case [x;y] -> x * o case endmatch",
            lambdaEval "0",
            "todo"
        )
        (
            "EDGE CASES 16",
            lambdaEval "let f = [1;2;3;4;5] in let g = [10;11;12;13;14] in match g case {x:y:z:a:b} -> let o = 0 in let q = [1;1;1;1;1] in match q case [x;y] -> x * o case endmatch case endmatch",
            lambdaEval "0",
            "todo"
        )
        (
         "Specific matches simple 1",
         lambdaEval "let f = [1 ; 2 ; 3 ; 4] in match f case { 1 : 2 : x : y } -> x + y case [] -> [] case endmatch",
         lambdaEval "3 + 4",
         "Variables should now be integer lits as they exact match"
        )
        
    ]

let testLambdaEvalDescriptions : (string * Result<AST,string> * Result<AST,string> * string) list= 
    [
        (
        "Lambda 1",
        lambdaEval "let rec fac n = if equals n 0 then 1 else n * fac ( n - 1 ) fi in fac 5",
        (Ok (Literal (Int 120L))),
        "Factorial function"
        )
        (
        "Lambda 2",
        lambdaEval "let g f x = f x x in g ( fun x y = x * y ) 3",
        (Ok (Literal (Int 9L))),
        "Anonymus function as argument"
        )   
        (
        "Lambda 3",
        lambdaEval "let rec fib n = if equals n 1 then 1 else if equals n 2 then 1 else fib ( n - 1 ) + fib ( n - 2 ) fi fi in fib 10",
        (Ok (Literal (Int 55L))),
        "Fibonacci function"
        )
        (
        "Lambda 4",
        lambdaEval "let rec lstmap func lst = if equals lst [ ] then [ ] else pair ( func ( fst lst ) ) ( lstmap func ( snd lst ) ) fi in lstmap ( fun x = x * 2 ) [ 1 ; 2 ; 3 ] ",
        (Ok (Pair(Literal(Int 2L),Pair(Literal(Int 4L),Pair(Literal(Int 6L),Null))))),
        "list.Map function"
        )
        (
        """Lambda 5: reverseword "abcdf" """,
        lambdaEval """let rec concatlist lsta lstb = if equals lsta [] then lstb else pair ( fst lsta ) ( concatlist ( snd lsta ) lstb )  fi in let rec reverselist lst = if equals lst [] then [] else concatlist ( reverselist ( snd lst ) ) ( [ fst lst ] ) fi in let reverseword str = implode ( reverselist ( explode str ) ) in reverseword "abcdef" """,
        (Ok (Literal (Str ['f'; 'e'; 'd'; 'c'; 'b'; 'a']))),
        "Concatenate, reverselist, reverseword"
        )
        (
        "Lambda 6: mutual recursion",
        [lambdaEval "mrec even n = if equals n 0 then true else odd ( n - 1 ) fi mrec odd n = if equals n 0 then false else even ( n - 1 ) fi" ; lambdaEval "[ even 101 ; odd 100 ; even 30 ; odd 31 ]"].[1],
        (Ok (Pair(falseAST,Pair(falseAST,Pair(trueAST,Pair(trueAST,Null)))))),
        "Even and odd functions"
        )
        (
        "Lambda 7: Lazy evaluation test",
        lambdaEval "let f x = x in f ( lazy ( let g x = x * x in g 2 ) )",
        (Ok (Literal (Int 4L))),
        "The lazy argument appears in the function body, and therefore it will be evaluated (in normal order)"
        )             
        (
        "Lambda 8: Lazy evaluation test",
        lambdaEval "let f x y = x in f 1 ( lazy ( let rec infinite x = infinite ( x+1 ) in infinite 0 ) )",
        (Ok (Literal (Int 1L))),
        "The infinite recursive function in the argument is never evaluated, and therefore the program terminates successfully"
        )                
    ]

