import Data.List
import Data.List.Split
import Data.Char (isSpace, isDigit)
import Data.Maybe

type BagColor = String
type Rule = (BagColor, [(Int, BagColor)]) 

count p = length . filter p
trim = dropWhileEnd isSpace . dropWhile isSpace

-- my god, what have i done
parseRule :: String -> Rule
parseRule s = (bagColor, contents)
  where
    [colorPart, contentsPart] = splitOn "contain" s
    [c1, c2, _, _] = splitOn " " colorPart
    bagColor = c1 ++ " " ++ c2
    no = head $ splitOn " " $ trim contentsPart
    contents = if no == "no" then [] else map (parseContents.trim) $ splitOn "," contentsPart
    parseContents x = 
      let [n, cc1, cc2, _] = splitOn " " x
       in (read n, cc1 ++ " " ++  cc2)

-- jesus christ forgive me, this is *embarassingly* slow
canContainShinyGoldBag :: [Rule] -> BagColor -> Bool
canContainShinyGoldBag rules bag = 
  ("shiny gold" `elem` directContents) || 
    (or $ map (canContainShinyGoldBag rules) directContents)
  where
    directContents = map snd $ fromJust $ lookup bag rules

bagsContained :: [Rule] -> BagColor -> Int
bagsContained rules bag = 
  (sum $ map fst $ directContents) + 
  (sum $ map (\(n, c) -> n*(bagsContained rules c)) $ directContents)
  where
    directContents = fromJust $ lookup bag rules

part1 :: [String] -> Int
part1 l = count (canContainShinyGoldBag rules) $ nub $ map fst rules
  where rules = map parseRule l

part2 :: [String] -> Int
part2 l = bagsContained rules "shiny gold"
  where rules = map parseRule l

main = do
  input <- readFile "input.txt"
  print $ part1 $ lines input
  print $ part2 $ lines input
