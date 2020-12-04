Oof.

Part 1 of the puzzle I solved relatively quickly: with some help from the `split` package,
accumulate all sections of the input separated by blank lines (representing all data from
a single passport), then split each field by spaces, then split the keys and values by a
colon delimiter, then count which passports have all required keys. I think in total the
solution was about 30 lines.

Then I got to part 2.

As is often the case for all of us when programming, I had a choice: go the quick and
dirty route, or take a less-quick, less-dirty approach. 

The problem is: at this point, I have a bunch of lists of key-value pairs, both of which
are strings. Depending on the key, I need to apply some validation to the value. The shape
of the values is highly variable, and some of the validations depend not just on the shape
but in the actual value itself. Parsing the strings by shape with, for example regular
expressions, then converting to whatever type I needed seemed not only cumbersome but also
kinda overkill.

So I went with the less-quick, less-dirty approach: model the field as a sum type, use
good ol' pattern matching (and string functions) to chop up and convert strings into the
types I needed.

The most didactic part of this excercise was learning how to chain functions that could
"fail" in a concise way, such that failing to do one step would lead to a failure in the
overall process. Since I am still (sort of) a Haskell beginner, my initial intuition told
me that this would probably require a lot of explicit pattern matching. 

Then I remembered [The
Typeclassopedia](https://wiki.haskell.org/wikiupload/e/e9/Typeclassopedia.pdf)

```haskell
(<$>) :: Functor f => (a -> b) -> f a -> f b
(=<<) :: Monad m => (a -> m b) -> m a -> m b
```

Since `Maybe` is a monad (and therefore also a functor), I can use `<$>` to 'lift' any
function into a function that fails if its input was also a faiulre. This includes type
constructors, naturally. I can also use `=<<` (which I used instead of `>>=` for some
inexplicable reason, probably because of the right-to-left nature of the usual function
application i.e. values flow from right to left in Haskell, and idiomatic code tends to
use lots of `.` and `$` which encourage reading functions right-to-left) to take the
result of a function that can fail as input for another function that can fail, such that
if the first function fails then the second one fails as well. 

After realizing this, all I needed was to:

- model validation of some property as success/failure:
  ```haskell
  validate :: (a -> Bool) -> a -> Maybe a
  ```
- model conversion of a string to some other type as success/failure:
  ```haskell
  safeRead :: Read a => String -> Maybe a
  ```

Then the rest was busywork, i.e. actually writing each individual validation.

(Disclaimer: I also used `<*>` once despite the fact that I still don't quite understand
how it works. If you're somehow actually reading this and know a simple explanation for
`<*>` please let me know.)

Implementing this left me with one small problem: since parsing a field implicitly means
validating its value, I can't work with parsed fields to solve part 1, since part 1
doesn't concern itself with values at all, only with keys. I ended up splitting the input
parsing process into two parts: first a function that takes the input and splits it into
lists of pairs of strings, and one that takes a list of pairs of strings and parses it
into a field. This way, part 1 can deal only with the partially processed input, and part
2 deals with the fully parsed input.



#### Notes
- Pattern matching is awesome, but can make functions a little hard to read sometimes.
