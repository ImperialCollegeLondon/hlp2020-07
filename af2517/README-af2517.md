Parser information:

There is only one module for the parser named ParserModule. The input is the output of the tokenizer, and the module interfaces with the runtimes through a call to the function "parsedOutput". This is the function that is called after tokenizing the input and returns the appropriate AST or Error Message that will then be passed to the Runtimes. 

There are a few extensions to the MVP such as support for "* / + -" operators, left associativity in function applications,anonymous functions, lists of: literals, functions (with or without operators) and other lists, using the built in type pair. And finally, recursive functions parsing. 

There is another extension that supports match statements in an F# way and this was entierly done by Vlad Nistor (VN1417). This could have been implemented in a very similar way to lists but because of time constraints the code couldn't be joined and it is implemented in a different way. I also received a bit of general debugging help from Vlad as he had to understand my code to implement his extension. 

The Token types were given at the beginning by Vlad and some of them are currently unused, but might be implemented later in the group phase. With regards to the AST types, Elliott (EJ3517) and Daniel (DG2217), responsible for the combinator and lambda runtimes respectively, have worked closely with me since the beginning and when changes were required, I have adjusted the types to help them and avoid major changes later. These adjustments include a Lazy AST type that indicates an expression should be evaluated lazily, or adding an extra Math type for mathematical built in functions. In conclusion, the types have been generaly set by the other modules and I have adapted them to their needs. Lastly, we decided in conjunction to parse if statements in a church boolean style (as function applications) with Lazy arguments. 

Initially, in function definitions the type FuncDefExp was used, which requires an expression where the function is evaluated. If an expression was not given by the user an error was thrown. However, due to the FSI-Like extension carried out by Daniel, the parser accepts function definitions without an expression that evaluates them. In this case, such expressions correspond to a different type in the AST named FuncDef. 

The changes needed to join the Parser with the module at the bottom (the tokenizer) and the one at the top (the runtimes) should be minimal as we have a working version of the code that joins the three levels together, used to speed up testing. 
