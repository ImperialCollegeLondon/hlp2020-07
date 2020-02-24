module TokenModule 


open System
type Lexer = char list -> (char list * char list) option
type Token = OpenRoundBracket
            |CloseRoundBracket
            |OpenSquareBracket
            |CloseSquareBracket
            |IntegerLit of int
            |StringLit of string
            |SpaceLit
            |DecimalLit of float
            |Keyword of string
            |Let
            |RightArrow
            |Equal
            |HexLit of string
            |NegativeInteger of string
            |Add
            |Multiply
            |Other of string
            |Substract
            |Div
            |Unexpected
//Rules
//------------------------------------------------------------
let integerLit =
    [['0'..'9'],true]
let negIntegerLit =
    [
        ['-'],false
        ['0'..'9'],true

    ]
let stringLit =
    [['\"'],false
     ['0'..'9']@['a'..'z']@['A'..'Z']@[' '],true
     ['\"'],false]
let decimalLit =
    [
        ['0'..'9'],true
        ['.';','],false
        ['0'..'9'],true
    ]
let negDecimalLit =
    [
        ['-'],false
        ['0'..'9'],true
        ['.';','],false
        ['0'..'9'],true
    ]
let spaceLit = 
    [
        [' '],true
    ]
let openRoundBracketLit = 
    [
        ['('],false
    ]
let closeRoundBracketLit = 
    [
        [')'],false
    ]
let openSquareBracketLit = 
    [
        ['['],false
    ]
let closeSquareBracketLit = 
    [
        [']'],false
    ]
let keywordLet = 
    [
        ['l'],false
        ['e'],false
        ['t'],false
    ]
let keywordRightArrow = 
    [
        ['-'],false
        ['>'],false
    ]
let keywordEqual = 
    [
        ['='],false
    ]
let binaryLit = 
    [
        ['0'],false
        ['x'],false
        ['A'..'F'] @ ['0'..'9'] @ ['a'..'f'],true
    ]
let addition = 
    [
        ['+'],false
    ]
let multiplication = 
    [
        ['*'],false
    ]
let div = 
    [
        ['/'],false
    ]
let substract = 
    [
        ['-'],false
    ]
let otherCharacters = 
    [
        ['a'..'z'],true
    ]
//------------------------------------------------------------
//Dict
//------------------------------------------------------------
let mdict = [
             substract
             negIntegerLit
             integerLit
             stringLit
             decimalLit
             negDecimalLit
             spaceLit
             openRoundBracketLit
             closeRoundBracketLit 
             openSquareBracketLit
             closeSquareBracketLit
             keywordLet
             keywordRightArrow
             keywordEqual
             binaryLit  
             addition
             multiplication
             otherCharacters
             div
             ]
//------------------------------------------------------------
//Returns first occurence of element in list
let index arr elem =
    let rec find arr elem pos = 
        match arr with 
            |hd::_ when hd = elem -> pos
            |_::tl -> find tl elem (pos + 1)
            |[] -> failwithf "Couldn't find element"
    find arr elem 0


   
let print x = 
    printfn "%A" x
let lexNGram (ngram: ((char list * bool) list)   ) (cLst: char list) : (char list * char list) option =
    //a single character belongs to this rule
    let takeIfInChars chars (acc,lst) : (char list * char list) option =
        match lst with
        |hd::tl ->
            if List.contains hd chars then Some (acc @ [hd],tl) else None
        |[] -> Some (acc, lst)
    //multiple characters belong to a rule
    let rec takeWhileInChars (acc,lst) chars  : (char list * char list) option =
        //Make sure to check if you can get at least one before trying more
        match lst with
        |hd::tl ->
            if List.contains hd chars then takeWhileInChars (acc @ [hd],tl) chars else Some (acc ,lst)
        |[] -> Some (acc,lst)
    //Every iteration of this consumes a rule
    let tryMatch (state: (char list * char list) option ) (charsLst,canRepeat) : ((char list * char list) option ) =
        match state with
        //Notice how as soon as you fail the rule you just completely fail
        |Some s -> if canRepeat then
                        let s' = takeWhileInChars s charsLst
                        if s' = Some s then None else s'
                    else
                        takeIfInChars charsLst s
        |None -> None
    //Remember if you do it this way then it's interpreted as folding over ngram
    ((Some ([],cLst)) , ngram) ||> List.fold tryMatch
//Must incorporate some error reporting/checking
let combinedLexers (mstring:string) : ((char list * int) list) = 
    //be worth safeguarding against rules that have not been added somehow
    let tryRule (rule:(char list * bool) list) (look: char list) : ((char list * int) * char list) option = 
        match lexNGram rule look with
            |Some (acc,left) -> Some ( (acc, index mdict rule),left )
            |None -> None
    let tryAllRules  (look:char list) :  ((char list * int) * char list) option = 
        let miniFolder state (rule : (char list * bool) list) =
            match tryRule rule look with 
                | None -> state // it failed try the next one
                | Some x -> x 
        Some (((([],-1),[]), mdict) ||> List.fold miniFolder) 


    let rec consume (acc: (char list * int) list) (look:char list) : ((char list * int) list) =
        match tryAllRules look with 
            |Some ( x,y ) when y <> [] ->  consume (acc @ [x]) y
            |Some (x,y) when List.isEmpty y -> acc @ [x]
            |_ -> failwithf "What? A rule must've not matched"
    consume [] (Seq.toList mstring)
let tokenize (mstring:string) : Token list = 
    let flattener (x:char list,y:int) : Token = 
        match y with 
            | 0 -> Substract
            | 1 | 2 -> IntegerLit (int (x |> Array.ofList |> String))
            | 3 -> StringLit (x |> Array.ofList |> String) 
            | 4 | 5 -> DecimalLit (float (x |> Array.ofList |> String))
            | 6 -> SpaceLit
            | 7 -> OpenRoundBracket
            | 8 -> CloseRoundBracket
            | 9 -> OpenSquareBracket
            | 10 -> CloseSquareBracket
            | 11 -> Let
            | 12 -> RightArrow
            | 13 -> Equal
            | 14 -> HexLit (x |> Array.ofList |> String)
            | 15 -> Add 
            | 16 -> Multiply
            | 17 -> Other (x |> Array.ofList |> String)
            | 18 -> Div
            | _ -> Unexpected
    mstring
    |> combinedLexers
    |> List.map flattener 

