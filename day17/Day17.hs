import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

data Point3D = Point3D (Int,Int,Int) deriving (Eq, Ord, Show)
data Point4D = Point4D (Int,Int,Int,Int) deriving (Eq, Ord, Show)

parseGrid :: Ord a => [[(a, Char)]] -> Set a
parseGrid = Set.fromList . map fst . filter ((=='#').snd) . concat

parseGrid3D :: [String] -> Set Point3D
parseGrid3D = parseGrid 
              . zipWith (\n l -> zipWith (\m c -> (Point3D (n,m,0),c)) [0..] l) [0..]

parseGrid4D :: [String] -> Set Point4D
parseGrid4D = parseGrid
              . zipWith (\n l -> zipWith (\m c -> (Point4D (n,m,0,0),c)) [0..] l) [0..]

-- generate all neighbors to a point (by adding or substracting 1 from each
-- coord or leaving it alone)
neighbors3D :: Point3D -> Set Point3D
neighbors3D  (Point3D p@(a,b,c)) = Set.fromList
  [Point3D (a+x, b+y, c+z) | x <- l, y <- l, z <- l , (a+x, b+y, c+z) /= p]
    where l = [-1,0,1]

neighbors4D :: Point4D -> Set Point4D
neighbors4D (Point4D p@(a,b,c,d)) = Set.fromList
  [Point4D (a+x,b+y,c+z,d+w) | x <- l, y <- l, z <- l, w <- l, (a+x,b+y,c+z,d+w) /= p]
    where l = [-1,0,1]

-- Neighborable is a typeclass for types for which you can find a set of
-- neighbors. this allows us to reuse both `neighborCount` and `step`
-- bellow for Point3D and Point4D
class Ord a => Neighborable a where
  neighbors :: a -> Set a

instance Neighborable Point3D where
  neighbors = neighbors3D

instance Neighborable Point4D where
  neighbors = neighbors4D

-- for each point, generate a map from its neighbors to 1,
-- then combine all maps by adding their values together,
-- such that at the end we have a map from each point to its number of 
-- active neighbors.
neighborCount :: Neighborable a => Set a -> Map a Int
neighborCount points = Map.unionsWith (+) ns
  where ns = [Map.fromSet (const 1) (neighbors p) | p <- Set.toList points]

-- the set of next active points is the union of:
-- * all currently active points with 2 or 3 active neighbors, and
-- * all currently inactive points with exactly 3 active neighbors
step :: Neighborable a => Set a -> Set a
step points = Set.union old new
  where old = Map.keysSet $ 
              Map.filterWithKey (\k n -> (k `Set.member` points) && ((n==2)||(n==3))) $ 
                count
        new = Map.keysSet $
              Map.filterWithKey (\k n -> (not (k `Set.member` points)) && (n==3)) $
                count
        count = neighborCount points

part0 :: Neighborable a => Set a -> Int
part0 = Set.size . (!!6) . iterate step

part1 :: [String] -> Int
part1 = part0 . parseGrid3D

part2 :: [String] -> Int
part2 = part0 . parseGrid4D

main = do
  input <- readFile "input.txt"
  print $ part1 $ lines input
  print $ part2 $ lines input
