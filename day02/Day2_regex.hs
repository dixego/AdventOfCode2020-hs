import Text.Regex.TDFA
import Day2

passwordRegex = "([0-9]+)-([0-9]+) ([a-zA-Z]): ([a-zA-Z]+)"

parsePassword :: String -> Password
parsePassword passwd = Password (read mi) (read mj) (ch!!0) p
  where
    (_,_,_,[mi, mj, ch, p]) = passwd =~ passwordRegex :: (String, String, String, [String])

main = do
  input <- readFile "input.txt"
  let parsedPasswords = map parsePassword $ lines input
  print $ part1 $ parsedPasswords
  print $ part2 $ parsedPasswords
