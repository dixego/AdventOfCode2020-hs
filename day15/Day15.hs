{-# LANGUAGE BangPatterns #-}
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
import Data.List
import Data.List.Split

(!?) :: IntMap a -> Int -> Maybe a
(!?) = flip Map.lookup

continueSequence :: [Int] -> [Int]
continueSequence init = init ++ map (\(x,_,_) -> x) (iterate' step start)
  where 
    start = (0, length init + 1, Map.fromList $ zip init [1..])

    -- does using !mem here even do anything? something tells me it doesn't
    step (lastN, turn, !mem) = case mem !? lastN of
      Nothing -> (0, turn+1, Map.insert lastN turn mem)
      Just n  -> (turn-n, turn+1, Map.insert lastN turn mem)

part0 :: Int -> String -> Int
part0 n = get n . continueSequence . map read . splitOn ","
  where get = flip (!!)

part1 :: String -> Int
part1 = part0 2019

part2 :: String -> Int
part2 = part0 29999999

main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input

