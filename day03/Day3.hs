import Data.Bool (bool)

-- go down the grid taking `is` steps to the right and `js` steps down
-- counting how many trees (#) are found along the way
sledDown :: Int -> Int -> [[Char]] -> Int
sledDown is js grid = sum $ zipWith foundTree grid' cols {-go 0 grid-}
  where
    {---explicit recursion solution. looks nicer imo.
    go _ []        = 0 
    go i g@(x:_)   = foundTree x i + go (inc i) (skip g)
    -}

    grid'          = map head $ takeWhile (not.null) $ iterate skip grid
    cols           = iterate inc 0 

    foundTree x i  = bool 0 1 $ x!!i == '#'
    inc            = flip mod len . (+is)
    skip           = drop js 
    len            = length $ grid!!0

-- find number of trees while going down sled 3 left and 1 down
part1 :: [[Char]] -> Int
part1 = sledDown 3 1

-- find product of a series of different traversals
part2 :: [[Char]] -> Int
part2 g = product $ map ($ g) traversals
  where traversals = [
          sledDown 1 1, 
          sledDown 3 1, 
          sledDown 5 1, 
          sledDown 7 1, 
          sledDown 1 2
          ]

main = do
  input <- readFile "input.txt"
  let grid = lines input
  print $ part1 grid
  print $ part2 grid
