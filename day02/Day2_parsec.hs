import Text.Parsec hiding (count)
import Text.Parsec.Char
import Data.Either
import Day2

type Parser = Parsec String ()

number :: Parser Int
number = read <$> (many1 digit)

pass :: Parser Password
pass =
  Password <$>
    number <*  char '-' <*> number <*  space <*> letter <*  char ':' <*  spaces <*> (many1 letter)

main = do
  input <- readFile "input.txt"
  let parsedPasswords = map (parse pass "") $ lines input
  print $ part1 $ rights $ parsedPasswords
  print $ part2 $ rights $ parsedPasswords
