import Data.List (union, intersect, nub)
import Data.List.Split (splitWhen)

part0 :: ([a] -> [a] -> [a]) -> [[a]] -> Int
part0 f = sum . map (length . foldl1 f) . splitWhen null

part1 :: [String] -> Int
part1 = part0 union

part2 :: [String] -> Int
part2 = part0 intersect

main = do
  input <- readFile "input.txt"
  print $ part1 $ lines input
  print $ part2 $ lines input
