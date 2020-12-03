On day 3 (while still technically during day 2 for me) I tried to solve the problem as
quickly as possible, just to see if I could get on the leaderboards. I finished the
solution found in `Day3.py` in about eleven minutes, which was about nine minutes too many
to be among the first. Oh well. 

I don't think I'll be attempting that in the future; programming at 11 pm is not exactly
easy or pleasant.

The problem itself is quite easy thanks to the magic of the modulo operator. First I came
up with an explicit recursion solution. I then remembered once reading that one should try
to avoid explicit recursion, so I came up with a solution using `zipWith` and `iterate`.
Honestly, I still find the explicitly recursive version to be easier to read.

#### Notes

- I love the `bool` operator.
- I also love defining inner functions. It's so nice.
