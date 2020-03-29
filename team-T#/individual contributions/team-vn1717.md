# VN1717 - Individual Contributions


Since individual phase I have written the tests in Program.fs that relate to matches and finished integrating matches into the language. The integration of matches into the language since the individual phase consisted of

* Rewriting how the parser operates on matches (The way it was done before was still written by me but a newer way had to be used that separated the pattern the user gave and the cases the user wanted to match against)
* Writing the runtime for single values matches e.g: variables such as 1 or "hello"
* Writing the runtime for general list matches e.g. match x case [x;y]
* Writing the runtime for specific list matches e.g. match x case {x:y}
* Writing the tests associated with the helper functions required for the above to work as well as the tests associated with matches

As a good rule of thumb, if it's to do with matches it was written by me. 

