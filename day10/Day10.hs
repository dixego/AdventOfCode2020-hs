import Data.List
import Data.List.Split

count p = length . filter p
pairs = zip <*> tail

completeSortedAdapters :: [Int] -> [Int]
completeSortedAdapters ad = sort $ (0:ad) ++ [maximum ad + 3]

nbonnaci n = go
  where go = take n (repeat 0) ++ [1] ++ z n
        z 1 = zipWith (+) go (drop 1 go)
        z x = zipWith (+) (z (x-1)) (drop x go)

tri :: Int -> Int
tri n = nbonnaci 2 !! (n+2)

part1 :: [String] -> Int
part1 input = ones * threes
  where ones     = count (1==) diffs
        threes   = count (3==) diffs 
        diffs    = map (uncurry $ flip (-)) $ pairs complete
        complete = completeSortedAdapters numbers
        numbers  = map read input
        pairs    = zip <*> tail

part2 :: [String] -> Int
part2 input = 
  product $ map (tri.length) $ filter (not.null) $ splitOn [3] diffs
  where diffs = map (uncurry $ flip (-)) $ pairs complete
        complete = completeSortedAdapters numbers
        numbers = map read input

main = do
  input <- readFile "input.txt"
  print $ part1 $ lines input
  print $ part2 $ lines input
