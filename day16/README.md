Jesus Christ what a messy solution this turned out to be!

Part 1 was simple enough, just filter all the tickets with invalid values, find the
invalid values and sum them all together.

For part 2 I mapped every "field" of every ticket to a list of possible fields they could
fit in, then once I had all possible fields for each column I noticed one of them had only
one possibility, so I could discard that one possible field from all other columns. Repeat
this process for every column with only one possibility until only one possibility remains
for each column. 

It would probably look *way* more elegant on an imperative language, and there's probably
a much prettier way to do it in Haskell, but for now I'm satisfied with this solution.
