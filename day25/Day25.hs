transform :: Int -> Int -> Int
transform s v = (v*s) `mod` 20201227

crack :: Int -> Int
crack c = snd $ until (\(v, t) -> v == c) (\(v,t) -> (transform 7 v, t+1)) (1, 0)

genEncryptionKey :: Int -> Int -> Int
genEncryptionKey pk ls = (!!ls) $ iterate (transform pk) 1

part1 :: String -> Int
part1 input = genEncryptionKey key2 $ crack key1
  where [key1, key2] = map read $ lines input

main = do
  input <- readFile "input.txt"
  print $ part1 input
