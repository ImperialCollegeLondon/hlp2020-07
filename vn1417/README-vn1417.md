How will your code be used by team (if all goes well) including what order are modules in your team project compiled?


1) My tokenizer converts a string to the tokens it contains. This will be the very first Module compiled. (The Definition.fs and then TokenModule.fs). I've split them such that all the logic is in one place and all the definitions are in another.
2) My "match" code in the parser will be used to parse match statements

Which parts if any are code written for other people?

I've written the 'match' code in the parser - this includes the AST definitions (of the match) and logic flow of the match statement throughout the parser code. 

Which parts if any of code you use is written by others?

When implementing the match statement I had to use the backbone of the parser and extend on it.

How did you work out (who decided what - how do you revise) the types that interface your code with others?

Initially we had a language archtiecture in mind with certain keywords we wanted. As we progressed in our code we realised some keywords that we didn't think of needed to be added. (Also, as we extended the language further we needed more keywords so we just added them and communicated to the concerned parties the exact token they should expect (for the parser) or the exact AST to expect (for the runtime))

What help have you obtained/given others debugging or doing code review?

Had a meetup to advise on code readability and functional-mindset. 
Also helped debug in the parser (the bug was that certain tokens were thrown away due to the recursive nature but it since been resolved)
