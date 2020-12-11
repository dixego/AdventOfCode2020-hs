import Data.Set (Set)
import Data.Vector (Vector, (!))
import qualified Data.Set as Set
import qualified Data.Vector as V
import Text.Parsec 
import Text.Parsec.Char
import Data.Either

data Instruction = Acc Int | Jmp Int | Nop Int deriving Show
type Program = Vector Instruction

type Parser = Parsec String ()

signedInteger :: Parser Int 
signedInteger = read <$> (pos <|> neg)
  where pos = char '+' *> number
        neg = (:) <$> char '-' <*> number
        number = many1 digit

instruction :: Parser Instruction
instruction = (parseAcc <|> parseJmp <|> parseNop)
  where parseAcc = Acc <$> inst "acc"
        parseJmp = Jmp <$> inst "jmp"
        parseNop = Nop <$> inst "nop"
        inst s = string s *> space *> signedInteger

parseProgram :: [String] -> Program
parseProgram = V.fromList . rights . map (parse instruction "")

-- evaluates an instruction returning the new program counter and accumulator
eval :: Int -> Int -> Instruction -> (Int, Int)
eval pc acc (Acc i) = (pc+1, acc+i)
eval pc acc (Jmp i) = (pc+i, acc)
eval pc acc (Nop i) = (pc+1, acc)

programLoops :: Program -> Bool
programLoops prog = pc /= V.length prog 
  where (pc, _, _) = runProgramUntilLoop 0 0 (Set.empty) prog

-- swap jump for nop or nop for jump at instruction `n` 
swapJmpNop :: Int -> Program -> Program
swapJmpNop n prog = V.update prog $ V.singleton (n, subst $ prog ! n) 
    where subst (Jmp i) = Nop i
          subst (Nop i) = Jmp i
          subst (Acc i) = Acc i

-- eval program updating accumulator and program counter until it executes
-- the last instruction or it visits a previously visited instruction
runProgramUntilLoop :: Int -> Int -> Set Int -> Program -> (Int, Int, Set Int)
runProgramUntilLoop pc acc ran program
  | V.null program = (pc, acc, ran)
  | Set.member pc ran = (pc, acc, ran)
  | pc >= V.length program = (pc, acc, ran)
  | otherwise = let (pc', acc') = eval pc acc $ program ! pc in 
                  runProgramUntilLoop pc' acc' (Set.insert pc ran) program

-- Find accumulator value at point that program repeats an instruction
part1 :: [String] -> Int
part1 s = acc
  where (_, acc, _) = runProgramUntilLoop 0 0 (Set.empty) $ parseProgram s

-- Find accumulator value by chainging an instruction so the program no longer loops
part2 :: [String] -> Int
part2 s = acc
  where (_, acc, _) = runProgramUntilLoop 0 0 Set.empty prog'
        prog' = head $ dropWhile programLoops $ zipWith swapJmpNop [0..] $ repeat prog
        prog = parseProgram s

main = do
  input <- readFile "input.txt"
  print $ part1 $ lines input
  print $ part2 $ lines input
