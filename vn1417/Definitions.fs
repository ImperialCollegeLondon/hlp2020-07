module Definitions
open System

type Lexer = char list -> (char list * char list) option
type MappedRule = (char list * int)

//char list in the following definition represents the characters left to be mapped to a rule
//option due to implementation
type AccumulatedMappedRule = (MappedRule * char list) option
type Rule = (char list * bool) list

let print x =
    printfn "%A" x

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
             keywordColon
             ]



