import Text.Parsec
import Linear.V3
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Either (rights)
import Data.Foldable (toList)

type Parser = Parsec String ()
type Coord = V3 Int

data Direction = E | SE | SW | W | NW | NE deriving (Show)

move :: Direction -> Coord -> Coord
move E  = (+ (V3 1 (-1) 0))
move W  = (+ (V3 (-1) 1 0))
move NE = (+ (V3 1 0 (-1)))
move NW = (+ (V3 0 1 (-1)))
move SE = (+ (V3 0 (-1) 1))
move SW = (+ (V3 (-1) 0 1))

parseLine :: Parser [Direction]
parseLine = many1 parseDirection
  where parseDirection = choice $ map try dirs
        dirs = zipWith (<$) [E,W,NE,NW,SE,SW] $ map string ["e","w","ne","nw","se","sw"]

parseInput :: String -> [[Direction]]
parseInput = rights . map (parse parseLine "") . lines

followDirections :: Coord -> [Direction] -> Coord
followDirections = foldl (flip move)

freqs :: (Foldable f, Ord a) => f a -> Map a Int
freqs = Map.fromListWith (+) . flip zip (repeat 1) . toList

neighbors :: Coord -> Set Coord
neighbors c = Set.fromList $ map (flip move c) [E, W, NE, NW, SE, SW]

neighborCount :: Set Coord -> Map Coord Int
neighborCount tiles = Map.unionsWith (+) ns
  where ns = [Map.fromSet (const 1) (neighbors t) | t <- Set.toList tiles]

step :: Set Coord -> Set Coord
step bs = old <> new 
  where ns = neighborCount bs
        old = Map.keysSet.Map.filterWithKey (\k v -> Set.member k bs && (v == 1||v == 2)) $ ns
        new = Map.keysSet.Map.filterWithKey (\k v -> Set.notMember k bs && v==2) $ ns

part0 :: String -> Map Coord Int
part0 = freqs . map (followDirections $ pure 0) . parseInput

part1 :: String -> Int
part1 = Map.size . Map.filter odd . part0

part2 :: String -> Int
part2 = Set.size . (!!100) . iterate step . Map.keysSet . Map.filter odd . part0

main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input
