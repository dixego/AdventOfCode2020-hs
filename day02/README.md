Ahh, the hardest problem in computer science: parsing strings into data structures.

It took me about 20 minutes to come up with the solution to the actual problem. Then I
spent about three hours researching how to parse a string to turn it into the data I
needed.

(I have no doubt in my mind that there was some easier way to do it).

My first thought was: "this would be trivial in Python with a regular expression". My
second thought was: "wait, don't Haskellers really like parser combinator libraries?".

So I went with the second thought first. As the name implies, `Day2_parsec.hs` uses Parsec to parsethe input into a data structure. I'm always a bit confused whenever using applicative style for anything, but I think it came out pretty OK. Self-documenting even.

After I was done with that I still wanted to know if the regex approach would work.
`Day2_regex.hs` uses `regex-tdfa` to parse the data using a regular expression. Ignoring
the external dependency (and the fact that `regex-tdfa` supports almost no character
classes), it's almost as simple as the Parsec implementation. It is, however, also a bit
slower.

`Day2.hs` contains the solution to the actual problem, which is pretty much just two
simple validity tests given a password and its policies.

If I had to pick, I'd say the Parsec approach is probably more easily modifiable than the regex
approach, if only because after obtaining the regex match you still have to parse a list
of strings whereas with Parsec you can build a big parser for a complex data structure out
of small parsers for its simpler parts. I may be wrong, though.
