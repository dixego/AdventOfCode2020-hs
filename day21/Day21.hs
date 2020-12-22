import Text.Parsec
import Text.Parsec.Char
import Data.Either (rights)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Data.List (sortOn, intercalate)
import Data.Bool (bool)

type Parser = Parsec String ()

ingredientsAllergens :: Parser ([String], [String])
ingredientsAllergens = (,) <$> ingredients <*> (option [] $ between oPar cPar $ string "contains" *> space *> allergens)
  where
    word = many1 letter
    wordsSpace = word `sepEndBy` space
    wordsComma = word `sepBy` string ", "
    ingredients = wordsSpace
    oPar = char '('
    cPar = char ')'
    allergens = wordsComma

parseInput :: [String] -> [([String], [String])]
parseInput = rights . map (parse ingredientsAllergens "")

potentialAllergenIngredients :: [([String], [String])] -> Map String (Set String)
potentialAllergenIngredients = Map.unionsWith (Set.intersection) . map flatten
  where flatten (ings , als) = Map.fromList $ [(al, Set.fromList ings) | al <- als]

ingredientSet :: [([String], [String])] -> Set String
ingredientSet = Set.fromList . concatMap fst

counter :: Ord a => [a] -> Map a Int
counter = Map.unionsWith (+) . map (flip Map.singleton 1)

valueSet :: Ord a => Map k (Set a) -> Set a
valueSet = Map.foldl (<>) Set.empty

reducePossibilities :: Ord b => Map a (Set b) -> Map a (Set b)
reducePossibilities m = Map.map deleteSingle m
  where singleValues =  valueSet $ Map.filter ((==1).Set.size) m
        deleteSingle s = if Set.size s == 1 then s else s \\ singleValues

getAllergenIngredients :: Ord b => Map a (Set b) -> Map a (Set b)
getAllergenIngredients = until (\m -> all (==1) $ Map.map (Set.size) m) reducePossibilities

part1 :: [String] -> Int
part1 input = Map.foldl (+) 0 $ (counter $ concatMap fst ls) Map.\\ dangerous
  where ls = parseInput input
        iSet = ingredientSet ls
        aMap = potentialAllergenIngredients ls
        igCounter = counter $ concatMap fst ls
        dangerous = Map.fromSet id $ valueSet aMap

part2 :: [String] -> String
part2 input = intercalate "," $ concatMap (Set.toList . snd) $ sortOn fst $ Map.toList dangerous
  where 
    ls = parseInput input
    aMap = potentialAllergenIngredients ls
    dangerous = getAllergenIngredients aMap

main = do
  input <- readFile "input.txt"
  print $ part1 $ lines input
  print $ part2 $ lines input
