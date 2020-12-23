import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable (toList)
import Data.List.Split (splitOn)
import Data.Either (either)

type Deck = Seq Int
type Game = (Deck, Deck)
type History = Set Game

parseInput :: String -> Game
parseInput = (\[x,y] -> (x,y)) . map (Seq.fromList . parseDeck) . splitOn "\n\n"
  where parseDeck = map read.tail.lines

gameStep :: Game -> Game
gameStep g@(Empty, _) = g
gameStep g@(_, Empty) = g
gameStep (a :<| as, b :<| bs)
  | a >= b = (as :|> a :|>b, bs)
  | otherwise = (as, bs :|> b :|> a)

game :: Game -> Deck
game = (\(a,b) -> if Seq.null a then b else a) . until (\(a,b) -> Seq.null a || Seq.null b) gameStep

recursiveGame :: History -> Game -> Either Deck Deck
recursiveGame _ (d1, Empty) = Left d1
recursiveGame _ (Empty, d2) = Right d2
recursiveGame hs g@(a :<| as, b :<| bs)
  | Set.member g hs = Left (a :<| as)
  | Seq.length as >= a && Seq.length bs >= b = 
      case recursiveGame Set.empty (Seq.take a as, Seq.take b bs) of
        Left  _ -> recursiveGame (Set.insert g hs) (as:|>a:|>b, bs)
        Right _ -> recursiveGame (Set.insert g hs) (as, bs:|>b:|>a)
  | otherwise = recursiveGame (Set.insert g hs) (gameStep g)

score :: Deck -> Int
score = sum . zipWith (*) [1..] . reverse . toList 

part1 :: String -> Int
part1 = score . game . parseInput

part2 :: String -> Int
part2 = either score score . recursiveGame Set.empty . parseInput

main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input
