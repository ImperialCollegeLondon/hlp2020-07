module reWrite

open Lambdas


let pairToString p =
    match p with
    | 
    | Null

let rec reWrite ast = 
    match ast with 
    | Literal (Int number) -> string number
    | Literal (String charList) -> charList |> List.map string |> List.reduce (+)
    | Pair(a,b) -> "[" @ pairToString Pair(a,b) @ "]"
    | Null -> "[]"


