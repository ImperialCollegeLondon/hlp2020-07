open Expecto
open TokenModule
open Definitions
open System.IO


let print x =
    printfn "%A" x

let testDescriptions = [
    ("match",tokenize "\n match other \n",[Newline;SpaceLit;Keyword "match";SpaceLit;Other "other";SpaceLit;Newline],"tokenize '\\n match other \\n' -> [Newline;SpaceLit;Keyword \"match\";SpaceLit;Other \"other\";SpaceLit;Newline]")
    ("No Input",tokenize "", [NoInput],"tokenize \"\" -> NoInput")
    ("Single new line lit",tokenize "\n",[Newline],"tokenize \\n -> Newline")
    ("Newline lit in the middle",tokenize "; \n ;",[Keyword ";";SpaceLit;Newline;SpaceLit ;Keyword ";"],"tokenize \n -> newlineLit")
    ("Single Integer Lit",tokenize "21", [IntegerLit 21] ,"tokenize 21 -> IntegerLit 21");
    ("Single Decimal Lit",tokenize "21.2",[DecimalLit 21.2],"tokenize 21.2 -> DecimalList 21.2")
    ("Zero integer", tokenize "0", [IntegerLit 0], "tokenize 0 -> IntegerLit 0")
    ("Zero Decimal Lit", tokenize "0.0", [DecimalLit 0.0], "tokenize 0.0 -> DecimalLit 0.0")
    ("Zero negative integer", tokenize "-0", [IntegerLit 0], "tokenize -0 -> IntegerLit 0")
    ("Zero negative Decimal Lit", tokenize "-0.0", [DecimalLit 0.0], "tokenize -0.0 -> DecimalLit 0.0")
    ("Single Div symbol",tokenize "/",[DivToken],"tokenize / -> Div")
    ("Single Other",tokenize "other",[Other "other"],"tokenize other -> Other \"other\"")
    ("All symbols", tokenize "-> - / * + let ( ) [ ] 21.2 -21.2 21 -21 = \n 0xffaa other \"Hello There \"",
                                                [
                                                    RightArrow
                                                    SpaceLit
                                                    SubToken
                                                    SpaceLit
                                                    DivToken
                                                    SpaceLit
                                                    MultToken
                                                    SpaceLit
                                                    AddToken
                                                    SpaceLit
                                                    Let
                                                    SpaceLit
                                                    OpenRoundBracket
                                                    SpaceLit
                                                    CloseRoundBracket
                                                    SpaceLit
                                                    OpenSquareBracket
                                                    SpaceLit
                                                    CloseSquareBracket
                                                    SpaceLit
                                                    DecimalLit 21.2
                                                    SpaceLit
                                                    DecimalLit -21.2
                                                    SpaceLit
                                                    IntegerLit 21
                                                    SpaceLit
                                                    IntegerLit -21
                                                    SpaceLit
                                                    EqualToken
                                                    SpaceLit
                                                    Newline
                                                    SpaceLit
                                                    HexLit "0xffaa"
                                                    SpaceLit
                                                    Other "other"
                                                    SpaceLit
                                                    StringLit "\"Hello There \""                                         
                                                ],                                         
                                                "tokenize -> - / * + let ( ) [ ] 21.2 -21.2 21 -21 = \\n 0xffaa other \"Hello There \"")
    ]

let makeMyTests (x,y,z,name) = 
      test x {Expect.equal y z name}

[<Tests>]
let makeem = testList "A test group" (List.map makeMyTests testDescriptions)

//This actually caught -1 (i.e. NEGATIVE NUMBERS) failing!
let properties =
  testList "FsCheck samples" [
    testProperty "Integer Lit works for any integer" <| fun a ->
      tokenize (string a) = [IntegerLit a]
    ]

let mList = [1;2;3]

[<EntryPoint>]
let main argv =
    //print <| tokenize "-> - / * + let ( ) [ ] 21.2 -21.2 21 -21 = 0xffaa other \"Hello There \""
    runTestsInAssembly defaultConfig [||] |> ignore
    (*let readLines =
              "/Users/vladnistor/Documents/dev/hlp2020-07/vn1417/Input.txt"
              |> File.ReadAllLines
              |> Array.toList
              
    print readLines
    *)
    //print <| Seq.toList "\n"
    //print <| tokenize "= \n 0xffaa other \"Hello There \""
    //Tests.runTests defaultConfig properties |> ignore
    //print <| tokenize "21.2"
    //print "Hi "
    0
    

    
    
(*

expected:
[RightArrow; SpaceLit; SubToken; SpaceLit; DivToken; SpaceLit; MultToken;
 SpaceLit; AddToken; SpaceLit; Let; SpaceLit; OpenRoundBracket; SpaceLit;
 CloseRoundBracket; SpaceLit; OpenSquareBracket; SpaceLit; CloseSquareBracket;
 SpaceLit; DecimalLit 21.2; SpaceLit; DecimalLit -21.2; SpaceLit; IntegerLit 21;
 SpaceLit; IntegerLit -21; SpaceLit; EqualToken; SpaceLit; HexLit "0xffaa";
 SpaceLit; Other "other"; SpaceLit; StringLit ""Hello There ""]
  actual:
[RightArrow; SubToken; DivToken; MultToken; AddToken; Let; OpenRoundBracket;
 CloseRoundBracket; OpenSquareBracket; CloseSquareBracket; DecimalLit 21.2;
 DecimalLit -21.2; IntegerLit 21; IntegerLit -21; EqualToken; HexLit "0xffaa";
 Other "other"; StringLit ""Hello There ""]

                                             
*)

