import Data.List.Split
import Data.List
import Data.Ord
import Data.Maybe

type BusId = Int
type Time = Int

parseBusIds :: String -> [BusId]
parseBusIds = map read . filter (/="x") . splitOn ","

minutesRemaining :: Time -> BusId -> Int
minutesRemaining time id = id - (time `mod` id)

-- note: modInv and gcdExt are ripped wholesale from Rosetta Code. thx.
-- I am not good at modular arithmetic or algebra in general.
modInv :: Int -> Int -> Maybe Int
modInv a m 
  | 1 == g = Just (mkPos i)
  | otherwise = Nothing
  where (i, _, g) = gcdExt a m
        mkPos x
          | x < 0 = x + m
          | otherwise = x

gcdExt :: Int -> Int -> (Int, Int, Int)
gcdExt a 0 = (1, 0, a)
gcdExt a b = let (q, r) = a `quotRem` b 
                 (s, t, g) = gcdExt b r 
              in (t, s-q*t, g)

parseEquations :: String -> [(Int, Int)]
parseEquations = map (\(a,m) -> let m' = read m in ((-a)`mod`m', m')) . 
                 filter ((/="x").snd) . 
                 zip [0..] . 
                 splitOn ","

equationSolution :: [(Int, Int)] -> Int
equationSolution eq = solution `mod` n
  where
    as = map fst eq
    ms = map snd eq
    n = foldl1 (*) ms 
    ys = map (n `div`) ms 
    zs = catMaybes $ zipWith modInv ys ms 
    solution = sum $ zipWith3 (\a b c -> a*b*c) as ys zs

part1 :: [String] -> Int
part1 input = 
  uncurry (*) $ 
  minimumBy (comparing snd) $ 
  map (\b -> (b, minutesRemaining time b)) buses
  where time = read $ input!!0
        buses = parseBusIds $ input!!1

part2 :: [String] -> Int
part2 = equationSolution . parseEquations . (!!1)

main = do
  input <- readFile "input.txt"
  print $ part1 $ lines input
  print $ part2 $ lines input
