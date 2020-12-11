import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Data.Maybe
import Data.List (tails, find)

-- O(n^2) babyyyy!
-- except technically since the preamble is always length 25
-- this is actually O(1) :)  
isSumOfElements :: Int -> [Int] -> Maybe (Int, Int)
isSumOfElements n xs = listToMaybe $ [(x, y)| x <- xs, y <- xs, x/=y, x+y==n]

findWeakness :: [Int] -> [Int] -> Maybe Int
findWeakness _ [] = Nothing
findWeakness pre@(x:xs) (y:ys) 
  | isJust sum = findWeakness (xs++[y]) ys
  | otherwise = Just y
  where sum = isSumOfElements y pre

findContiguousSum :: Int -> [Int] -> Maybe [Int]
findContiguousSum n xs
  | (snd $ last interval) == n = Just $ take (fst $ last interval) xs
  | otherwise = Nothing
  where interval = zip [1..] $ takeWhile (<=n) $ scanl1 (+) xs

part1 :: [String] -> Int
part1 = fromJust . uncurry findWeakness . splitAt 25 . map read 

part2 :: [String] -> Int
part2 input = (maximum interval + minimum interval)
  where numbers = map read input
        wrong = part1 input
        Just interval = fromJust $ find isJust $ map (findContiguousSum wrong) $ tails numbers

main = do
  input <- readFile "input.txt"
  print $ part1 $ lines input
  print $ part2 $ lines input
