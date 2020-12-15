import Data.Map hiding (map, foldr)
import qualified Data.Map as Map (map, foldr)
import Data.Either
import Text.Parsec
import Text.Parsec.Char

type Parser = Parsec String ()

data Instruction = Mask [Int] | Assign Int [Int] deriving Show
type Memory = Map Int [Int]

instruction :: Parser Instruction
instruction = try assign <|> mask
  where mask = (Mask . parseMask) <$> (string "mask" *> spaces *> char '=' *> spaces *> many (oneOf "X01"))
        assign = Assign <$> mem <*> value 
        mem = read <$> (string "mem" *> char '[' *> many1 digit <* char ']' <* spaces <* char '=' <* spaces)
        value = (toBin36 . read) <$> (many1 digit)

toBin :: Int -> [Int]
toBin = reverse . toBin'
  where toBin' 0 = [0]
        toBin' 1 = [1]
        toBin' n = n `mod` 2 : toBin' (n `div` 2)

toBin36 :: Int -> [Int]
toBin36 n = replicate (36 - length l) 0 ++ l
  where l = toBin n

fromBin36 :: [Int] -> Int
fromBin36 = fromBin36' . reverse
  where fromBin36' [] = 0
        fromBin36' (x:xs) = x + (2 * fromBin36' xs)

parseMask :: String -> [Int]
parseMask = map maskDigit
  where maskDigit 'X' = -1
        maskDigit '1' = 1
        maskDigit '0' = 0

applyMask1 :: [Int] -> [Int] -> [Int]
applyMask1 = zipWith mask
  where mask 0 _    = 0
        mask 1 _    = 1
        mask (-1) n = n

-- generates multiple binary numbers depending on the mask
applyMask2 :: [Int] -> [Int] -> [[Int]]
applyMask2 m i = f $ zipWith mask' m i
  where mask' 0 n = n
        mask' 1 _ = 1
        mask' (-1) _ = -1
        f [] = [[]]
        f (0:xs) = map (0:) $ f xs
        f (1:xs) = map (1:) $ f xs
        f ((-1):xs) = let xs' = f xs in (map (0:) xs') ++ (map (1:) xs')

parseInstructions :: [String] -> [Instruction]
parseInstructions = rights . map (parse instruction "")

eval1 :: Memory -> [Int] -> [Instruction] -> Memory
eval1 mem _ [] = mem
eval1 mem _ ((Mask n):is) = eval1 mem n is
eval1 mem mask ((Assign m v):is) = eval1 (insert m (applyMask1 mask v) mem) mask is

-- inserts `value` at all `keys`
multiInsert :: [Int] -> [Int] -> Memory -> Memory
multiInsert keys value mem = foldr (\k m -> insert k value m) mem keys

eval2 :: Memory -> [Int] -> [Instruction] -> Memory
eval2 mem _ [] = mem
eval2 mem _ ((Mask n):is) = eval2 mem n is
eval2 mem mask ((Assign m v):is) = eval2 (multiInsert (map fromBin36 (applyMask2 mask (toBin36 m))) v mem) mask is

type Evaluator = Memory -> [Int] -> [Instruction] -> Memory

part0 :: Evaluator -> [String] -> Int
part0 ev = Map.foldr (\v n -> n + (fromBin36 v)) 0 . ev empty [] . parseInstructions

part1 :: [String] -> Int
part1 = part0 eval1

part2 :: [String] -> Int
part2 = part0 eval2

main = do
  input <- readFile "input.txt"
  print $ part1 $ lines input
  print $ part2 $ lines input
