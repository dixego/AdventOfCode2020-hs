A mathematical puzzle, neat!

To be fair, I could only tell part 2 could be solved via the Chinese Residue Theorem by
writing down what finding the solution would mean:

_So if at time `t` the bus with ID 7 arrives, then at time `t+1` arrives bus 13, then at
time `t+4` arrives bus 59..._

```
t = 0 mod 7 
t + 1 = 0 mod 13
t + 4 = 0 mod 59
t + 6 = 0 mod 31
t + 7 = 0 mod 19
```

_...wait a second, isn't there some theorem about how to solve modular equations like
this?_

So I looked it up and there was indeed a theorem.

The algorithm I implemented for solving the equation system was adapted from
[here](https://brilliant.org/wiki/chinese-remainder-theorem/). The code for getting the
modular inverse I [ripped wholesale from Rosetta
Code](https://rosettacode.org/wiki/Modular_inverse#Haskell) since it looked simple enough
but I was sure I'd mess something up if I tried writing it myself.

### Notes
- Thanks, Rosetta Code.
- There was also another way to find the solution by searching linearly but increasing the
  search step evey time you found a time that matched two buses. I don't know if I would
  have arrived at that solution myself had I not seen the modular equation system.
