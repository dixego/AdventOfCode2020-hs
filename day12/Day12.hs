data Instruction =
    N Int
  | S Int
  | E Int
  | W Int
  | L Int
  | R Int
  | F Int deriving (Show)

type Instructions = [Instruction]

data Direction = North | South | East | West deriving (Show)

type Position = (Int, Int)

parseInstruction :: String -> Instruction
parseInstruction ('N':s) = N (read s)
parseInstruction ('S':s) = S (read s)
parseInstruction ('E':s) = E (read s)
parseInstruction ('W':s) = W (read s)
parseInstruction ('L':s) = L (read s)
parseInstruction ('R':s) = R (read s)
parseInstruction ('F':s) = F (read s)

clock :: Direction -> Direction
clock North = East
clock East  = South
clock South = West
clock West  = North

counter :: Direction -> Direction
counter = clock . clock . clock

turn :: Instruction -> Direction -> Direction
turn (L n) d = iterate counter d !! (n `div` 90)
turn (R n) d = iterate clock d !! (n `div` 90)
turn _ d = d

eval :: Position -> Direction -> Instruction -> (Position, Direction)
eval (x,y) d (N n) = ((x, y+n), d)
eval (x,y) d (S n) = ((x, y-n), d)
eval (x,y) d (W n) = ((x+n, y), d)
eval (x,y) d (E n) = ((x-n, y), d)
eval (x,y) d (F n) = (forwards (x,y) d n, d)
  where forwards (x,y) North n = (x,y+n)
        forwards (x,y) East n  = (x-n,y)
        forwards (x,y) South n = (x,y-n)
        forwards (x,y) West n  = (x+n,y)
eval (x,y) d i     = ((x,y), (turn i d))

followInstructions :: Position -> Direction -> Instructions -> (Position, Direction)
followInstructions p d = foldl (uncurry eval) (p, d)

clock' :: (Int, Int) -> (Int, Int)
clock' (x,y) =  (y, -x)

counter' :: (Int, Int) -> (Int, Int)
counter' = clock' . clock' . clock'

turn' :: Instruction -> Position -> Position
turn' (L n)  p = iterate counter' p !! (n `div` 90)
turn' (R n)  p = iterate clock' p !! (n `div` 90)
turn' _      p = p

eval' :: Position -> Position -> Instruction -> (Position, Position)
eval' s (wx, wy) (N n) = (s, (wx, wy+n))
eval' s (wx, wy) (S n) = (s, (wx, wy-n))
eval' s (wx, wy) (W n) = (s, (wx-n, wy))
eval' s (wx, wy) (E n) = (s, (wx+n, wy))
eval' s w i@(L n) = (s, turn' i w)
eval' s w i@(R n) = (s, turn' i w)
eval' s@(sx, sy) w@(wx, wy) (F n) = ((sx+(n*wx), sy+(n*wy)), w)

followInstructions' :: Position -> Position -> Instructions -> (Position, Position)
followInstructions' s w = foldl (uncurry eval') (s, w)

manhattanD :: (Int, Int) -> Int
manhattanD (x,y) = abs x + abs y

part1 :: [String] -> Int
part1 = manhattanD . fst . followInstructions (0,0) East . map parseInstruction

part2 :: [String] -> Int
part2 = manhattanD . fst . followInstructions' (0,0) (10,1) . map parseInstruction

main = do
  input <- readFile "input.txt"
  print $ part1 $ lines input
  print $ part2 $ lines input
