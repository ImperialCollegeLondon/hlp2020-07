module TokenModule 
open System
open Definitions

//Returns first occurence of element in list
let index arr elem =
    let rec find arr elem pos = 
        match arr with 
            |hd::_ when hd = elem -> pos
            |_::tl -> find tl elem (pos + 1)
            |[] -> failwithf "Couldn't find element"
    find arr elem 0

let lexNGram (ngram: Rule   ) (cLst: char list) : (char list * char list) option =
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
    
let combinedLexers (mstring:string) : (MappedRule list) = 

    //will throw an error if the rule is not in the dictionary    
    let tryRule (rule: Rule) (look: char list) : AccumulatedMappedRule = 
        match lexNGram rule look with
            |Some (acc,left) -> Some ( (acc, index mdict rule),left ) //might throw error here
            |None -> None
            
    //Note how it'll return the result of the last rule that matched - by design
    //Must ensure dict contains the most specific rule last
    //i.e Token '->' should come after Token '-'  
    let tryAllRules  (look:char list) :  AccumulatedMappedRule = 
        let ruleFolder (state:AccumulatedMappedRule) (rule : Rule) : AccumulatedMappedRule =
            match tryRule rule look with 
                | None -> state // it failed try the next one
                | x -> x
        //mdict contains all the rules
        (None, mdict) ||> List.fold ruleFolder


    let rec consume (acc: MappedRule list) (look:char list) : (MappedRule list) =
        match tryAllRules look with 
            |Some ( x,y ) when y <> [] ->  consume (acc @ [x]) y
            |Some (x,y) when List.isEmpty y -> acc @ [x]
            |_ ->  acc @ [(look),-1]
            
            
    consume [] (Seq.toList (mstring + " ") )
    
    
    
    
let tokenize (mstring:string) : Token list = 
    let flattener ((x:char list,y:int) : MappedRule) : Token =
        let xString = (x |> Array.ofList |> String)
        match y with
            | 0 -> Other xString
            | 1 -> SubToken
            | 2 | 3 -> IntegerLit (int64 xString)
            | 4 -> StringLit xString
            | 5 | 6 -> DecimalLit (float xString)
            | 7 -> SpaceLit
            | 8 -> OpenCurlyBracket
            | 9 -> CloseCurlyBracket
            | 10 -> OpenRoundBracket
            | 11 -> CloseRoundBracket
            | 12 -> OpenSquareBracket
            | 13 -> CloseSquareBracket
            | 14 -> Let
            | 15 -> RightArrow
            | 16 -> EqualToken
            | 17 -> HexLit xString
            | 18 -> AddToken
            | 19 -> MultToken
            | 20 -> DivToken
            | 21 -> Newline
            | 22 -> Keyword "match"
            | 23 -> Keyword "endmatch"
            | 24 -> Keyword "case"
            | 25 -> Keyword "if"
            | 26 -> Keyword "then"
            | 27 -> Keyword "else"
            | 28 -> Keyword "fi"
            | 29 -> Keyword ":"
            | 30 -> if xString = "" then NoInput else Keyword  ";"
            | _ -> Unexpected xString
                
    let res =
        mstring
        |> combinedLexers
        |> List.map flattener
        |> List.filter (fun x -> x <> SpaceLit && x <> Newline)
    
    if List.isEmpty res then [NoInput] else res
   