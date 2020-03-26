# Sample README

NB - replace this file by your overall team README.

This directory is used for:

* Team phase code submission
* Team phase individual team contribution statements (named `team-login.md`)
* overall team readme



Docs for Matches
Value to match is a number 

let a = 5
match a case 21 -> 1 case 20 -> 2 case 5 -> 101 case endmatch ====> 101

Note how every case must start with a case, the value to match and then a right arrow. After the right arrow the result of the match is given if the value matches. 
Note also how every match must end with CASE ENDMATCH (not capital but capitalised here such that both words are emphasized).

Value to match is a list
You can use [x;y] notation same as one would use x :: y in F#
You can use {x:y} notiation same as one would use [x;y] in F# (apologies)
Note curly brackets as opposed to square brackets and the colon as opposed to semicolon.


let a = [1;2;3;4;5;6]

match a case [x;y] -> x case [] -> -1 case endmatch ====> 1
match a case [x] -> x case endmatch ====> [1;2;3;4;5;6]
match a case {x:y} -> x case [] -> -1 case [x;y] -> x case endmatch ====> 1



let b = [1;2]

match b case {x:y} -> x + y case [] -> -1 case [x] -> 2 case endmatch ====> 3
match b case {x} -> -1 case [x;y] -> 100 case [x] -> -5 case endmatch ====> 100 (note shadowing effect).


let c = []

match c case [x;y] -> -1 case [] -> 1 case [x] -> 5 case endmatch ====> 1
match c case [x;y] -> -1 case [x] -> 1 case [] -> 100 case endmatch ====> 1 ([x] shadows everything as expected)
 
