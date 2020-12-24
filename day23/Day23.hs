{-# LANGUAGE BangPatterns #-}
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.List

type Limits = (Int, Int)
type GameState = Seq Int

between a (lo, hi) = lo <= a && a <= hi
tupMap (f, g) a = (f a, g a)
pairs = zip <*> tail
(!) = Seq.index

parseInput1 :: String -> (Int, GameState)
parseInput1 input = (,) h $ foldl (\s (i,n) -> Seq.update i n s) s idxNum
  where 
    num = map (read . (:[])) $ init input
    (h,t) = (head num, last num)
    idxNum = (t,h):(pairs num)
    s = Seq.fromList [0..(length num)]

parseInput2 :: String -> (Int, GameState)
parseInput2 input = (leftmost,
   Seq.update (limit) leftmost 
  $ Seq.update rightmost (maxN+1)
  $ state' Seq.>< seq')
  where
    num = map (read . (:[])) $ init input
    (leftmost, state') = parseInput1 input
    rightmost = last num
    maxN = maximum num
    limit = (10^6)
    seq' = Seq.fromList [maxN+2..limit+1]

limits :: GameState -> Limits
limits = tupMap (max 1 . minimum, maximum)

move :: Limits -> Int -> GameState -> (Int, GameState)
move (lo, hi) n !cl = (newRight, cl')
  where 
    cl' = Seq.update c (cl!(newFocus n))
      $ Seq.update (newFocus n) a
      $ Seq.update n newRight cl
    newRight = cl!c
    removed@[a,b,c] = take 3 $ iterate (cl!) (cl!n)

    newFocus n = until (\n -> n`notElem` removed && n `between` (lo,hi))
                       (\n -> (n-1) `mod` (hi+1)) (n-1)

game :: Int -> Int -> GameState -> GameState
game n f cl = snd $ (!!n) $ iterate' (uncurry $ move lims) (f, cl)
  where lims = limits cl
  
part1 :: String -> String
part1 input = concatMap show $ takeWhile (/=1) $ iterate (gameResult!) $ (!1) gameResult
  where gameResult = (uncurry $ game 100) $ parseInput1 input

part2 :: String -> Int
part2 input = (gameResult ! 1) * (gameResult ! (gameResult! 1))
  where gameResult = (uncurry $ game (10^7)) $ parseInput2 input

main = do
  input <- readFile "input.txt"
  putStrLn $ part1 input
  print $ part2 input
