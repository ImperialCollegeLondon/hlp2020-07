Parser description:

There is only one module for the parser named ParserModule. The only function that interfaces the other modules is "parse". This is the function that is called after tokenizing the input and returns the appropriate AST that will then be passed to the Runtimes. 

There are a few extensions to the MVP such as support for lists, "* / + -" operators, left associativity in function applications and anonymous functions. 

There is another extension that supports match statements in an F# way and this was entierly done by Vlad Nistor (VN1417). I also received some general debugging help from Vlad as he had to understand my code to implement his extension. 

The Token types were given at the beginning by Vlad and some of them are currently unused, but might be implemented later in the group phase. With regards to the AST types, Elliott (EJ3517) and Daniel (DG2217), responsible for the combinator and lambda runtimes respectively, have worked closely with me since the beginning and when changes were required, I have adjusted the types to help them and avoid major changes later. These adjustments include a Lazy AST type that indicates an expression should be evaluated lazily, or adding an extra Math type for mathematical built in functions. In conclusion, the types have been generaly set by the other modules and I have adapted them to their needs. Lastly, we decided in conjunction to parse if statements in a church boolean style (as function applications) with Lazy arguments. The changes needed to join the Parser with the module at the bottom (the tokenizer) and the one at the top (the runtimes) should be minimal as we have a working (not finalized) version of the code that joins the three modules together, mainly used to speed up the runtime testing. 
