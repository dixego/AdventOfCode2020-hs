import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as Map
import Text.Parsec
import Text.Parsec.Char
import Data.List.Split (splitOn)
import Data.Either

type Parser = Parsec String ()

data Rule = Match Char | Choice [[Int]] deriving Show
type RuleSet = IntMap Rule

rule :: Parser (Int, Rule)
rule = try ruleMatch <|> ruleChoice
  where ruleMatch = (,) <$> numberColon <*> (Match <$> (between (char '"') (char '"') letter))
        ruleChoice = (,) <$> numberColon <*> ((Choice . filter (not.null) . (\(x,y) -> [x,y])) <$> listPair)
        listPair = (,) <$> (numberList ) <*> (option [] (char '|' *> space *> numberList))
        numberColon = (number <* char ':' <* spaces)
        number = read <$> (many1 digit)
        numberList = (number `sepEndBy` space)

parseRuleSet :: [String] -> RuleSet
parseRuleSet = Map.fromList . rights . map (parse rule "")

match :: RuleSet -> String -> Bool
match rs = any null . match' (rs!0)
  where match' _ [] = []
        match' (Match c) (x:xs)
          | c == x = [xs]
          | otherwise = []
        match' (Choice rss) xs = parsed
          where rules = map (map (rs!)) rss
                parsed = concat $ map (foldl (\strs rule -> concat $ map (match' rule) strs) [xs]) rules

part1 :: String -> Int
part1 input = matches
  where [rules, strings] = splitOn "\n\n" input
        rs = parseRuleSet $ lines rules
        matches = (length . filter id) $ map (match rs) $ lines strings

main = do
  input1 <- readFile "input.txt"
  input2 <- readFile "input2.txt"
  print $ part1 input1
  print $ part1 input2
