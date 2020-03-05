open Expecto
open TokenModule
open Definitions
open System.IO


let print x =
    printfn "%A" x


//**BUG**
//the keyword if shadows anything that starts with if -> ifinit -> keyword "if"; Other "init" !. FIX
let testDescriptions = [
    //Note how it's inserting a space; Is it necesarilly bad if it fails anyway?
    ("match if <", tokenize "match if <", [Keyword "match";Keyword "if";Unexpected "< "],"tokenize match if <")
    //Note how it's inserting a space; Is it necesarilly bad if it fails anyway?
    ("< match end if", tokenize "< match end if", [Unexpected "< match end if "],"tokenize < match end if")
    ("endmatchx \n\n", tokenize "endmatchx \n\n",[Other "endmatchx"; Newline; Newline],"endmatchx \\n\\n")
    ("Semicolon with space",tokenize "; ", [Keyword ";"],"tokenize ; _")
    ("Single semicolon", tokenize ";", [Keyword ";"],"tokenize ;")
    ("Match with space",tokenize "match ", [Keyword "match"],"tokenize match _")
    ("Single match",tokenize "match", [Keyword "match"],"tokenize match")
    ("ifinit elseinit theninit finit if else then fi other",tokenize "ifinit elseinit theninit finit if else then fi other",[Other "ifinit"; Other "elseinit"; Other "theninit";  Other "finit"; Keyword "if"; Keyword "else"; Keyword "then";  Keyword "fi"; Other "other"],"tokenize match 'ifinit elseinit theninit finit if else then fi other'")
    ("f -2", tokenize "f -2", [Other "f"; IntegerLit -2],"Substraction vs Application")
    ("f - 2", tokenize "f - 2", [Other "f"; SubToken; IntegerLit 2],"Substraction vs Application")
    ("match end match with semicolon inside",tokenize "match ; \n endmatch",[Keyword "match";Keyword ";";Newline;Keyword "endmatch"],"tokenize match ; \\n endmatch")
    ("match",tokenize "\n match other \n",[Newline;Keyword "match";Other "other";Newline],"tokenize '\\n match other \\n' -> [Newline;SpaceLit;Keyword \"match\";SpaceLit;Other \"other\";SpaceLit;Newline]")
    ("No Input",tokenize "", [NoInput],"tokenize \"\" -> NoInput")
    ("Single new line lit",tokenize "\n",[Newline],"tokenize \\n -> Newline")
    ("Newline lit in the middle",tokenize "; \n ;",[Keyword ";";Newline ;Keyword ";"],"tokenize \n -> newlineLit")
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
                                                    SubToken
                                                    DivToken
                                                    MultToken
                                                    AddToken
                                                    Let
                                                    OpenRoundBracket
                                                    CloseRoundBracket
                                                    OpenSquareBracket
                                                    CloseSquareBracket
                                                    DecimalLit 21.2
                                                    DecimalLit -21.2
                                                    IntegerLit 21
                                                    IntegerLit -21
                                                    EqualToken
                                                    Newline
                                                    HexLit "0xffaa"
                                                    Other "other" 
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
    //print <| tokenize "if a=b"
    //print <| mdict
    //print <| tokenize " ifinit match other "
    0
