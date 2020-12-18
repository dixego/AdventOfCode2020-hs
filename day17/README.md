Had to read up a bit on other people's solutions to get this one again! The key to this
one is to not try to simulate the entire grid a step at a time, for therein lies the path
to madness. It's easier to instead keep track of which points are active, accumulate how
many active neighbors the "accessible" neighborhood of each active point has, then
calculate the new set of active points from that. It's not even _that_ slow for 4D points.

I had a bit of fun defining a typeclass `Neighborable` for "types for which you can get
neighboring values", such that I could reuse the same step function for both `Point3D` and
`Point4D` (since I only needed to be able to get their set of neighboring points anyway).
