import Data.List  
import Data.Char (digitToInt)
import Data.Maybe

type Pass = (Int, Int) -- row, column

-- simple binary translation
decodePass :: String -> Pass
decodePass s = (row, col)
  where row = toDec $ translate $ take 7 s
        col = toDec $ translate $ drop 7 s
        translate = map subst
        subst 'F' = '0'
        subst 'B' = '1'
        subst 'L' = '0'
        subst 'R' = '1'
        toDec = foldl' (\a x -> a * 2 + digitToInt x) 0

passId :: Pass -> Int
passId (r,c) = 8*r+c

part1 :: [String] -> Int
part1 = maximum . map (passId.decodePass)

part2 :: [String] -> Int
part2 = (+1) . fst . fromJust . find (not.diffByOne) . pairs . sort . map (passId.decodePass)
  where pairs = zip <*> tail
        diffByOne = (== (-1)) . uncurry (-)

main = do
  input <- readFile "input.txt"
  let passes = lines input
  print $ part1 passes 
  print $ part2 passes 
