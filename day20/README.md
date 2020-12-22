OH MY GOD.

This is by far my longest solution so far, and the one that took me the longest to
complete (well over a day) as well.

Part 1 took me about an hour, thinking I was being clever by not assembling the actual
puzzle, but then I got to part 2 and debated on whether to actually do it or to skip it
and come back to it later. I ended up doing it, but the solution ain't pretty. Here's what
it does:

- Parse the input into tiles. A tile is represented as its ID, the numbers of their edges
  as per their binary interpretation, and the actual character grid.
- Generate an adjacency map from the tiles: two tiles are adjacent if some of their
  possible transformations (rotation, flipping) share edge numbers. This assumes edges are
  unique, which thankfully they seem to be.
- Find the corners from the adjacency map: a corner tile is only connected to two other
  tiles, i.e. the size of its adjacency set is 2.
- Pick an arbitrary corner. There must exist some transformation of the tile and its two
  adjacent tiles such that it can be placed on the top left corner (i.e., the tile's down
  edge coincides with one of its neighbors' up position, and its right edge coincides with
  one of its neighbors' left position. 
- Start building the grid from this top left corner. When adding a new tile, make sure
  that its left edge fits the right edge of the tile to its left, and its upper edge fits
  with the lower edge of the tile above it. Do this until all tiles are placed.
- Transform (flip, rotate) the matrix until you find the transformation that has sea
  monsters (thankfully there's only one such transformation). Count the sea monsters.

I was hesitant to finish this solution because I thought the code would end up being very
ugly, and it is indeed quite ugly. At least it's done now, though.
