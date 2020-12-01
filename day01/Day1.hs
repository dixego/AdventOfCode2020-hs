import Data.List
import Data.Maybe

-- no me gusta esto :c
pairs l = [[x,y] | x <- l, y <- l]
threes l = [[x,y,z] | x <- l, y <- l, z <- l]

findSum2020 :: [[Int]] -> Maybe [Int]
findSum2020 = find ((==2020).sum)

part0 :: [[Int]] -> Int
part0 = product . fromJust . findSum2020

-- Find the two entries that sum to 2020 and multiply them together
part1 :: [Int] -> Int
part1 = part0 . pairs

-- Find the THREE entries that sum to 2020 and multiply them together
part2 :: [Int] -> Int
part2 = part0 . threes

main = do
  input <- readFile "input.txt"
  let numbers = map read $ lines input :: [Int]
  print $ part1 numbers
  print $ part2 numbers
