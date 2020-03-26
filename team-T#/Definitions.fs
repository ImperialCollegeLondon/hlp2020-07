module Definitions
open System

let print x =
    printfn "%A" x



type Lexer = char list -> (char list * char list) option
type MappedRule = (char list * int)

//char list in the following definition represents the characters left to be mapped to a rule
//option due to implementation
type AccumulatedMappedRule = (MappedRule * char list) option
type Rule = (char list * bool) list

type Token = OpenRoundBracket
            |CloseRoundBracket
            |OpenSquareBracket
            |CloseSquareBracket
            |IntegerLit of int64
            |StringLit of string
            |SpaceLit
            |DecimalLit of float
            |Keyword of string
            |Let
            |RightArrow
            |EqualToken
            |HexLit of string
            |AddToken
            |MultToken
            |Other of string
            |SubToken
            |DivToken
            |Unexpected of string
            |Newline
            |NoInput
            |OpenCurlyBracket
            |CloseCurlyBracket

type MathType = 
   | Add 
   | Sub
   | Div
   | Mult
   | Mod
   | Greater
   | Lower

type BuiltInType = 
    | Mat of MathType
    | Equal //works for strings ints and nulls 
    | Explode 
    | Implode 
    | P //creates a pair 
    | PFst 
    | PSnd
    | IsPair
    | IfThenElse  
    | True | False
    | BS | BK | BI
    | Print

type AST = 
    | FuncDefExp of FuncDefExpType 
    | FuncDef of char list*AST
    | MutFuncDef of char list list * AST list
    | MatchDef of MatchDefType
    | Lambda of LambdaType
    | Var of char list //only valid in lambdas 
    | FuncApp of AST*AST
    | Pair of AST*AST
    | ExactPairMatch of AST*AST
    | Null 
    | Literal of LitType 
    | BFunc of BuiltInType
    | Bracket of AST
    | Y
    | Lzy of AST

and FuncDefExpType = {
    Name: char list;
    Body: AST
    Expression: AST
}

and LambdaType = {
    InputVar: char list
    Body: AST
}

and MatchDefType = {
    Condition: AST
    Cases: (AST*AST) list
}

and LitType = 
    | Int of int64 
    | Str of char list
    | UNIT


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
    [['"'],false
     ['0'..'9']@['a'..'z']@['A'..'Z']@[' '],true
     ['"'],false]
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
let hexLit = 
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
let keywordColon =
    [
        [';'],false
    ]
let newlineLit =
    [
        Seq.toList <| Environment.NewLine,false
    ]
    
let keywordMatch =
    [
        ['m'],false
        ['a'],false
        ['t'],false
        ['c'],false
        ['h'],false
        [' '],true
    ]
let keywordEndMatch =
    [
        ['e'],false
        ['n'],false
        ['d'],false
        ['m'],false
        ['a'],false
        ['t'],false
        ['c'],false
        ['h'],false
        [' '],true
    ]
    
let keywordCase = 
    [
        ['c'],false
        ['a'],false
        ['s'],false
        ['e'],false
        [' '],true
    ]
    
let keywordIf = 
    [
        ['i'],false
        ['f'],false
        [' '],true
    ]
let keywordThen = 
    [
        ['t'],false
        ['h'],false
        ['e'],false
        ['n'],false
        [' '],true
    ]
let keywordElse = 
    [
        ['e'],false
        ['l'],false
        ['s'],false
        ['e'],false
        [' '],true
    ]
let keywordFi = 
    [
        ['f'],false
        ['i'],false
        [' '],true
    ]
let openCurlyBracket = 
    [
        ['{'],false
    ]
let closeCurlyBracket =
    [
        ['}'],false
    ]
    
let keywordTwoDots =
    [
        [':'],false
    ]
    
//------------------------------------------------------------
//Dict
//------------------------------------------------------------
let mdict: Rule list = [
             otherCharacters
             substract
             negIntegerLit
             integerLit
             stringLit
             decimalLit
             negDecimalLit
             spaceLit
             openCurlyBracket
             closeCurlyBracket
             openRoundBracketLit
             closeRoundBracketLit
             openSquareBracketLit
             closeSquareBracketLit
             keywordLet
             keywordRightArrow
             keywordEqual
             hexLit  
             addition
             multiplication
             div
             newlineLit
             keywordMatch
             keywordEndMatch
             keywordCase
             keywordIf
             keywordThen
             keywordElse
             keywordFi
             keywordTwoDots
             keywordColon
             ]