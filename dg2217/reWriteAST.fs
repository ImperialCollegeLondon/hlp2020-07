module reWrite

open Lambdas

let rec pairToString p =
    match p with
    | Pair(a,b) -> string a + pairToString b
    | Null -> ""

let rec reWrite ast = 
    match ast with 
    | Literal (Int number) -> string number
    | Literal (String charList) -> charList |> List.map string |> List.reduce (+)
    | Pair(a,b) -> "[" + pairToString (Pair(a,b)) + "]"
    | Null -> "[]"


