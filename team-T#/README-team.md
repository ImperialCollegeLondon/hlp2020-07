# Sample README

NB - replace this file by your overall team README.

This directory is used for:

* Team phase code submission
* Team phase individual team contribution statements (named `team-login.md`)
* overall team readme

### Global comments 

* Make sure to space every new word or special character
* Neither capital letters nor numbers nor special character can be used in function names

### Built-In Types 

| Types         | Example          | Comments                                              |
| ------------- |:----------------:| :----------------------------------------------------:|
| strings       | "abcdefg"        | only lowercase, uppercase and numbers                 |
| integer       | 12345            | no floats                                             |
| lists         | [ 1 ; 2 ; 3 ; 5] | make sure to space all the semicolons and the entries.|
| empty list    | []               | /                                                     |

### Built-In Functions

|TSHARP Function  | F# Equivalent                               |
| --------------  |:-------------------------------------------:|
|+,-,*,/,mod      | +,-,*,/,%                                   |
| lower,greater   | <,>                                         |
| equals          | =                                           |
| fst,snd         | List.head, List.tail                        |
| pair hd tl      | hd::tl                                      |
| print           | printfn "%A" x                              |
| ispair          | checks if something is a list               |
| explode,implode | string to list of characters and vice versa |


### Function definition : 

let namefunc arga argb ... = ...

``` F#
let square x = x * x
print( square  3) ====> 9
```

``` F#
let func x = x - 2 * x in func 4 ====> -4
```

### lambda functions definition : 

fun arga argb ... = ...

``` F#
let addp f x = f x + f x
print ( addp ( fun x = x * 2 ) 3 ) ====> 12
```

### recursion functions : 
let rec namefunc arga argb ... = ...

``` F#
let rec increment x i = if equals x 10 then i else increment (x+1) (i+1) fi
print( increment -2 0 ) ====> 12
```

### mutual recursion functions must be written on the same line
mrec namefunca arga argb ... = ... mrec namefuncb argc argd ... = ...

``` F#
let rec fib n = if lower n 0 then ( "Input to fib should be positive" )  else if equals n 0 then 0 else if equals n 1 then 1 else fib ( n - 1 ) + fib ( n - 2 ) fi fi fi
print ( fib 55 ) ====> 365435296162
```

### if then else :
if ... then ... else ... fi

``` F#
let rec lstmap func lst = if equals lst [ ] then [ ] else pair ( func ( fst lst ) ) ( lstmap func ( snd lst ) ) fi
print ( lstmap ( fun x = x * x ) [ 1 ; 2 ; 5 ] ) ====> [ 1 ; 4 ; 25 ]
```

### Matches

Value to match is a number 

```F#
let a = 5
match a case 21 -> 1 case 20 -> 2 case 5 -> 101 case endmatch ====> 101
```

Note how every case must start with a case, the value to match and then a right arrow. After the right arrow the result of the match is given if the value matches. 
Note also how every match must end with CASE ENDMATCH (not capital but capitalised here such that both words are emphasized).

Value to match is a list
You can use [x;y] notation same as one would use x :: y in F#
You can use {x:y} notiation same as one would use [x;y] in F# (apologies)
Note curly brackets as opposed to square brackets and the colon as opposed to semicolon.

```F#
let a = [1;2;3;4;5;6]

match a case [x;y] -> x case [] -> -1 case endmatch ====> 1
match a case [x] -> x case endmatch ====> [1;2;3;4;5;6]
match a case {x:y} -> x case [] -> -1 case [x;y] -> x case endmatch ====> 1
```

```F#
let b = [1;2]

match b case {x:y} -> x + y case [] -> -1 case [x] -> 2 case endmatch ====> 3
match b case {x} -> -1 case [x;y] -> 100 case [x] -> -5 case endmatch ====> 100 (note shadowing effect).
```

```F#
let c = []

match c case [x;y] -> -1 case [] -> 1 case [x] -> 5 case endmatch ====> 1
match c case [x;y] -> -1 case [x] -> 1 case [] -> 100 case endmatch ====> 1 ([x] shadows everything as expected)
```

### Choosing Evaluation Order
let f x y = .... in f lazy(x) y 
* WILL EXECUTE ARGUMENT "x" IN NORMAL ORDER aka
* It will not evaluate "x" until it's found inside f's body

```F#
let f x y z k = print( x + y )
f ( lazy ( 2 ) ) 5 (lazy ( print ( "This will not be printed" ) ) )
```
