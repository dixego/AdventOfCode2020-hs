module Day2 where

data Password = Password {
  minOccurrences :: Int,
  maxOccurrences :: Int,
  character      :: Char,
  password       :: String
} deriving (Show)

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

-- first validity test: the count of character `ch` in `passwd` is between `mi` and `mj`
validPassword1 :: Password -> Bool
validPassword1 (Password mi mj ch passwd) = count (==ch) passwd `between` (mi, mj)
    where between n (lo, hi) = n >= lo && n <= hi

-- second validity test: either the `mi`-th or `mj`-th characters in `passwd` are `ch`
-- (but not both) (1-indexed)
validPassword2 :: Password -> Bool
validPassword2 (Password mi mj ch passwd) =
  passwd !! (mi-1) `is` ch /= passwd !! (mj-1) `is` ch
    where is = (==)

part1 :: [Password] -> Int
part1 = count validPassword1

part2 :: [Password] -> Int
part2 = count validPassword2
