open Expecto
open TokenModule


let testDescriptions = [
    ("Single Integer Lit",tokenize "21", [IntegerLit 21] ,"tokenize 21 -> IntegerLit 21");
    ("Single Decimal Lit",tokenize "21.2",[DecimalLit 21.2],"tokenize 21.2 -> DecimalList 21.2")
    ("Zero integer", tokenize "0", [IntegerLit 0], "tokenize 0 -> IntegerLit 0")
    ("Zero Decimal Lit", tokenize "0.0", [DecimalLit 0.0], "tokenize 0.0 -> DecimalLit 0.0")
    ("Zero negative integer", tokenize "-0", [IntegerLit 0], "tokenize -0 -> IntegerLit 0")
    ("Zero negative Decimal Lit", tokenize "-0.0", [DecimalLit 0.0], "tokenize -0.0 -> DecimalLit 0.0")
    ]

let makeMyTests descr = 
    match descr with
        |(x,y,z,name) -> test x {Expect.equal y z name}

[<Tests>]
let makeem = testList "A test group" (List.map makeMyTests testDescriptions)

//This actually caught -1 (i.e. NEGATIVE NUMBERS) failing!
let properties =
  testList "FsCheck samples" [
    testProperty "Integer Lit works for any integer" <| fun a ->
      tokenize (string a) = [IntegerLit a]
    ]





[<EntryPoint>]
let main argv =
    //runTestsInAssembly defaultConfig [||]
    //Tests.runTests defaultConfig properties
    print <| tokenize "21.2 -> let +"
    0
    

    
