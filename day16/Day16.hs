import Text.Parsec hiding (between)
import Text.Parsec.Char
import Data.List.Split (splitOn)
import Data.Either 
import Data.List ((\\), intersect, transpose, isPrefixOf, product)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace 
import Data.Maybe (fromJust)

between x (y, z) = y <= x && x <= z

type Parser = Parsec String ()
justParse :: Parser a -> String -> Either ParseError a
justParse = flip parse ""

word :: Parser String
word = many1 letter

wordsP :: Parser String
wordsP = concat <$> word `sepBy` spaces

number :: Parser Int
number = read <$> many1 digit

numbers :: Parser [Int]
numbers = number `sepBy` char ','

range :: Parser (Int, Int)
range = (,) <$> number <*> (char '-' *> number)

field :: Parser (String, [(Int, Int)])
field = (,) <$> wordsP <*> (char ':' *> spaces *> (range `sepBy` (string " or ")))

type Range = (Int, Int)
type Fields = Map String [Range]

valid :: Int -> [Range] -> Bool
valid x = any (x `between`) 

notValids :: [Int] -> [Range] -> [Int]
notValids nums ranges = filter (not . (`valid` ranges)) nums

containsNotValid :: [Int] -> [Range] -> Bool
containsNotValid nums ranges = length (notValids nums ranges) > 0

fitsFields :: Fields -> Int -> Fields
fitsFields fields value = Map.filter (valid value) fields

possibleFields :: Fields -> [Int] -> [String]
possibleFields fields values = foldl intersect (Map.keys fields) $ map (Map.keys . fitsFields fields) values

reducePossibilities :: [[String]] -> [[String]]
reducePossibilities possibleCols = reduced
  where
    singlePos = concat $ filter ((==1).length) possibleCols
    reduced = [ if length l /= 1 then l\\singlePos else l| l <- possibleCols]

getOrder :: [[String]] -> [String]
getOrder = concat . until (all ((==1).length)) reducePossibilities

part1 :: String -> Int
part1 input = sum allNotValid
  where [rangesS, ticketS, ticketsS] = splitOn "\n\n" input
        ranges = concat $ map snd $ rights $ map (justParse field) $ lines rangesS
        tickets = rights $ map (justParse numbers) $ lines ticketsS
        allNotValid = concat $ map (`notValids` ranges) tickets

part2 :: String -> Int
part2 input = product $ map snd $ filter (("departure"`isPrefixOf`).fst) $ zip colOrder myTicket
  where [rangesS, ticketS, ticketsS] = splitOn "\n\n" input
        
        ranges = rights $ map (justParse field) $ lines rangesS
        onlyRanges = concat $ map snd $ ranges

        fields = Map.fromList ranges

        tickets = rights $ map (justParse numbers) $ tail $ lines ticketsS
        Right myTicket = justParse numbers $ (lines ticketS) !! 1

        allValid = filter (not.(`containsNotValid` onlyRanges)) tickets
        
        byColumns = transpose allValid
        allPossibleFields = map (possibleFields fields) byColumns
        colOrder = getOrder allPossibleFields

main = do 
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input
