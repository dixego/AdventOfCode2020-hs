import Data.Matrix
import Data.List
import Data.Maybe

count p = length . filter p
while p = until (not.p)
pairs = zip <*> tail

adjacent :: (Int, Int) -> Matrix a -> Matrix a 
adjacent (x,y) m = submatrix (max 1 (x-1)) (min (nrows m) (x+1))
                             (max 1 (y-1)) (min (ncols m) (y+1)) m

occupiedAdjacent :: (Int,Int) -> Matrix Char -> Int
occupiedAdjacent c m = count (=='#') $ delete (m!c) $ toList $ adjacent c m

-- Ugh.
seenSeats :: (Int,Int) -> Matrix Char -> [Char]
seenSeats (x,y) m = filter (/='.') $ catMaybes $ map (\x -> (uncurry safeGet) x m) [vU, vD, vL, vR, vUL, vDR, vUR, vDL]
  where vU = while (\(x,y) -> x >= 1 && m!(x,y) == '.') (\(x,y) -> (x-1,y)) (x-1,y)
        vD = while (\(x,y) -> x <= (nrows m) && m!(x,y) == '.') (\(x,y) -> (x+1,y)) (x+1,y)
        vL = while (\(x,y) -> y >= 1 && m!(x,y) == '.') (\(x,y) -> (x,y-1)) (x,y-1)
        vR = while (\(x,y) -> y <=(ncols m) && m!(x,y) == '.') (\(x,y) -> (x,y+1)) (x,y+1)

        vUL = while (\(x,y) -> x >= 1 && y >= 1 && m!(x,y) == '.') (\(x,y) -> (x-1,y-1)) (x-1,y-1)
        vDR = while (\(x,y) -> x <= (nrows m)  && y <= (ncols m) && m!(x,y) == '.') (\(x,y) -> (x+1,y+1)) (x+1,y+1)
        vUR = while (\(x,y) -> x >= 1 && y <= (ncols m) && m!(x,y) == '.') (\(x,y) -> (x-1,y+1)) (x-1,y+1)
        vDL = while (\(x,y) -> x <= (nrows m)  && y >= 1 && m!(x,y) == '.') (\(x,y) -> (x+1,y-1)) (x+1,y-1)

updateSeats1 :: Matrix Char -> Matrix Char
updateSeats1 m = mapPos step m
  where step pos 'L'
          | occupiedAdjacent pos m == 0 = '#'
        step pos '#'
          | occupiedAdjacent pos m >= 4 = 'L'
        step _ c = c

updateSeats2 :: Matrix Char -> Matrix Char
updateSeats2 m = mapPos step m
  where step pos 'L'
          | (count (=='#') $ seenSeats pos m) == 0 = '#'
        step pos '#'
          | (count (=='#') $ seenSeats pos m) >= 5 = 'L'
        step _ c = c


part0 :: (Matrix Char -> Matrix Char) -> [String] -> Int
part0 step = count (=='#') . toList . snd . head . dropWhile (uncurry (/=)) . pairs . iterate step . fromLists

part1 :: [String] -> Int
part1 = part0 updateSeats1

part2 :: [String] -> Int
part2 = part0 updateSeats2

main = do
  input <- readFile "input.txt"
  print $ part1 $ lines input
  print $ part2 $ lines input
  
