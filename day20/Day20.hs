import Data.List
import Data.List.Split (splitOn)
import Data.IntMap (IntMap, (!), (!?))
import qualified Data.IntMap as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Matrix (Matrix)
import qualified Data.Matrix as Mx
import Data.Maybe

data Tile = Tile 
  { 
    tileId :: Int
  , up :: Int
  , right :: Int
  , down :: Int
  , left :: Int
  , grid :: [String]
  } deriving (Eq)

type TileGrid = Matrix Tile
type TileMap = IntMap Tile
type AdjacencyMap = IntMap (Set Int)

instance Show Tile where
  show (Tile i u r d l _) 
    = "([" ++ (show i) ++ "] " ++ (show u) ++ " " ++ (show r) ++ " " ++ (show d) ++ " " ++ (show l) ++ ")"

fromBin :: [Int] -> Int
fromBin = foldl (\n d -> d + 2*n) 0

toBin :: Int -> [Int]
toBin = reverse . toBin'
  where toBin' 0 = [0]
        toBin' 1 = [1]
        toBin' n = n `mod` 2 : toBin' (n `div` 2)

toBin10 :: Int -> [Int]
toBin10 n = replicate (10 - length l) 0 ++ l
  where l = toBin n

translate :: String -> [Int]
translate = map subst
  where subst '#' = 1
        subst '.' = 0

parseTile :: [String] -> Tile
parseTile tile = Tile tileN u r d l tiles
  where tileN = read $ head $ splitOn ":" $ last $ splitOn " " $ head tile
        tiles = tail tile
        trans = transpose tiles
        [u,d,l,r] = map (fromBin.translate) $ [head tiles, last tiles, head trans, last trans]

parseTiles :: String -> [Tile]
parseTiles = map parseTile . map (filter (not.null) . splitOn "\n") . splitOn "\n\n"

adjacencyMap :: [Tile] -> AdjacencyMap
adjacencyMap tiles = Map.fromList $ map findMatching tiles
  where findMatching t@(Tile i u r d l _) = (i, Set.fromList $ map tileId $ filter (matches t) tiles)
        matches t t' = 
            (tileId t) /= (tileId t') && 
              ((not . null) $ ((edges t) `intersect` (edges t')) ++ ((edges t) `intersect` (edges $ flipHV t')))

tileMap :: [Tile] -> TileMap
tileMap = Map.fromList . map (\t -> (tileId t, t))

transformations :: Tile -> [Tile]
transformations t = nub result
  where rotations = [id, rotateClock, rotateClock.rotateClock, rotateCounter]
        tiles = [t, flipH t, flipV t, flipHV t]
        result = concatMap (\t -> map ($t) rotations) tiles

neighbors :: TileMap -> AdjacencyMap -> Tile -> [Tile]
neighbors tMap aMap tile = [(tMap ! (id n)) | n <- Set.toList $ (aMap ! (tileId tile))]

topLeftConfiguration :: TileMap -> AdjacencyMap -> Tile -> (Tile, Tile, Tile)
topLeftConfiguration tMap aMap tile = (\(a,b,c) -> (a, fromJust b, fromJust c)) $ head $ filter (\(a,b,c) -> isJust b && isJust c) result
  where
    [n1, n2] = neighbors tMap aMap tile
    result = map (\t -> (t, 
                         listToMaybe $ (n1 `permSuchThat` ((==right t).left)), 
                         listToMaybe $ (n2 `permSuchThat` ((==down t).up)))) 
                 $ transformations tile

buildGrid :: [Tile] -> Matrix Tile
buildGrid tiles = Mx.mapPos (\_ v -> fromJust v) $ foldl setValue initMatrix positions
  where 
    tMap = tileMap tiles
    aMap = adjacencyMap tiles
    cs = corners tMap aMap
    (topLeft, rightC, downC) = topLeftConfiguration tMap aMap $ snd $ head $ Map.toList cs

    dim = truncate $ sqrt $ fromIntegral $ length tiles
    initMatrix' = Mx.matrix dim dim (\_ -> Nothing :: Maybe Tile)
    initMatrix = Mx.setElem (Just topLeft) (1,1) 
               $ Mx.setElem (Just rightC)  (1,2)
               $ Mx.setElem (Just downC)   (2,1) $ initMatrix'
    positions = [(x,y) | x <- [1..dim], y <- [1..dim], (x,y) `notElem` [(1,1), (1,2), (2,1)]]

    setValue m (r,c)
      | (r-1) < 1 = let Just lefter = m Mx.! (r, c-1)
                        nsLefter = neighbors tMap aMap lefter 
                        [candidate] = concatMap (`permSuchThat` ((==right lefter).left)) nsLefter
                     in Mx.setElem (Just candidate) (r,c) m
      | (c-1) < 1 = let Just upper = m Mx.! (r-1, c)
                        nsUpper = neighbors tMap aMap upper
                        [candidate] = concatMap (`permSuchThat` ((==down upper).up)) nsUpper
                     in Mx.setElem (Just candidate) (r,c) m
      | otherwise = let Just upper = m Mx.! (r-1,c)
                        Just lefter = m Mx.! (r, c-1)
                        ns = concatMap (neighbors tMap aMap) [upper, lefter]
                        candidate = concatMap (`permSuchThat` (\p -> (up p == down upper) && (left p == right lefter))) 
                                    $ nubBy (\x y -> tileId x == tileId y) ns 
                     in Mx.setElem (Just $ head candidate) (r, c) m

convertToSea :: Matrix Tile -> Matrix Char
convertToSea = Mx.fromLists . concat . map (foldl1 mergeGrids) . map (map cutBorders) . Mx.toLists
  where mergeGrids = zipWith (++)

findSeaMonster :: Matrix Char -> Bool
findSeaMonster mx = all (=='#') tiles
  where tiles = map (mx Mx.!) 
                $ [(1,19), 
                   (2,1), (2,6), (2,7), (2,12), (2,13), (2,18), (2,19), (2,20),
                   (3,2), (3,5), (3,8), (3,11), (3,14), (3,17)]
            
findSeaMonsters :: Matrix Char -> Int
findSeaMonsters mx = sum $ Mx.mapPos findMonsterSubMx mx
  where findMonsterSubMx (r,c) _
          | (r+2) <= dim && (c+19) <= dim = fromEnum $ findSeaMonster $ Mx.submatrix r (r+2) c (c+19) mx
          | otherwise = 0
        dim = Mx.ncols mx

matrixTransformations :: Eq a => Matrix a -> [Matrix a]
matrixTransformations t = nub result
  where rotations = [id, rotateClock', rotateClock'.rotateClock', rotateCounter']
        matrices = [t, flipH' t, flipV' t, flipHV' t]
        result = concatMap (\t -> map ($t) rotations) matrices 
        apply' f = Mx.fromLists . f . Mx.toLists
        flipH' = apply' (map reverse)
        flipV' = apply' reverse
        flipHV' = flipV' . flipH' 
        rotate'' = apply' (transpose . reverse)
        rotateClock' = (rotate'')
        rotateCounter' = ((!!3) . iterate rotateClock')

allSeaMonsters :: Matrix Char -> Int
allSeaMonsters = maximum . map findSeaMonsters . matrixTransformations

corners :: TileMap -> AdjacencyMap -> TileMap
corners tMap aMap = Map.filterWithKey (\k _ -> (Set.size (aMap!k)) == 2) tMap

permSuchThat :: Tile -> (Tile -> Bool) -> [Tile]
permSuchThat t p = filter p $ transformations t

flipH :: Tile -> Tile
flipH (Tile i _ _ _ _ g) = parseTile $ ["Tile " ++ (show i) ++ ":"] ++ (map reverse g)

flipV :: Tile -> Tile
flipV (Tile i _ _ _ _ g) = parseTile $ ["Tile " ++ (show i) ++ ":"] ++ (reverse g)

flipHV :: Tile -> Tile
flipHV = flipV . flipH

rotate' = transpose . reverse

rotateClock :: Tile -> Tile
rotateClock (Tile i _ _ _ _ g) = parseTile $ ["Tile " ++ (show i) ++ ":"] ++ (rotate' g)

rotateCounter :: Tile -> Tile
rotateCounter = (!!3) . iterate rotateClock

edges :: Tile -> [Int]
edges (Tile _ u r d l _) = [u,r,d,l]

cutBorders :: Tile -> [String]
cutBorders = map (init.tail) . init . tail . grid

part1 :: String -> Int
part1 input = product $ Map.keys cs
  where 
    tiles = parseTiles input
    tMap = tileMap tiles
    aMap = adjacencyMap tiles
    cs = corners tMap aMap

part2 :: String -> Int
part2 input = hashes - (15*monsters)
  where tiles = parseTiles input
        gr = buildGrid tiles
        sea = convertToSea gr
        monsters = allSeaMonsters sea
        hashes = foldl (\n c -> n + (fromEnum (c == '#'))) 0 sea

main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input
