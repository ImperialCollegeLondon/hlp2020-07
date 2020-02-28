How will your code be used by team (if all goes well) including what order are modules in your team project compiled?


My tokenizer converts a string to the tokens it contains. This will be the very first Module compiled. (The Definition.fs and then TokenModule.fs). I've split them such that all the logic is in one place and all the definitions are in another.

Which parts if any are code written for other people?

The tokenize function. Also I have an added folder in my directory called 'lex_parse'. This was/is being used by the other members to test their code. I've combined the tokenizer and parser such that the runtime people don't need to spell out long ASTs to test their code. 

Which parts if any of code you use is written by others?

I implemented the match statements in the parser. I used the types and helper PAPs to extend the functionality of the parser. 


How did you work out (who decided what - how do you revise) the types that interface your code with others?

Initially we had very basic types and as runtime/parsers needed more types we communicated the definition we want the previous module to give us. So for example the parser wanted certain Keywords as their own tokens as opposed to Keyword "keyword". I then added that. This is valid for runtimes as well.

What help have you obtained/given others debugging or doing code review?

Had a meetup to advise on code readability and functional-mindset.
