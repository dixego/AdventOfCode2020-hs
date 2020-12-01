This solution for day 1 is 

- quite concise
- a bit ugly
- O(n^2)

We generate the list of all pairs (or triples) of elements from a list then look at each
one until we find the one that adds up to 2020. 

Part 1 at least is solvable in O(n) (after sorting), however I cannot fathom the way to
translate indexing an array from both ends into something that wouldn't look hideous in
Haskell because it is very early in the morning. 

Also, hey, it's 26 lines! Surely expensive things are worth it if they're also pretty,
right (?).
