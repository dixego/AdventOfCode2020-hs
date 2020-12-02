import Data.List
import Data.Maybe

-- O(n). Find a pair of items in a list that sum up to k.
searchSum :: Int -> [Int] -> Maybe (Int, Int)
searchSum k l = searchSum' l (reverse l) k
  where searchSum' [] _ _ = Nothing
        searchSum' _ [] _ = Nothing
        searchSum' l@(x:xs) l'@(y:ys) k 
          | x + y == k = Just (x, y)
          | x + y < k  = searchSum' xs l' k
          | x + y > k  = searchSum' l ys k

-- Find the two entries that sum to 2020 and multiply them together
part1 :: [Int] -> Int
part1 = uncurry (*) . fromJust . searchSum 2020 . sort

-- Find the THREE entries that sum to 2020 and multiply them together
part2 :: [Int] -> Int
part2 l = 
    multiply 
  $ fromJust 
  $ find (isJust.snd) 
  $ map (\n -> (n, searchSum (2020-n) (sort l))) l
  where multiply (x, Just (y, z)) = x * y * z

main = do
  input <- readFile "input.txt"
  let numbers = map read $ lines input :: [Int]
  print $ part1 numbers
  print $ part2 numbers
