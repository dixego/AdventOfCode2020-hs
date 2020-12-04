import Data.List 
import Data.List.Split (splitOn, splitWhen)
import Data.Maybe
import Data.Char

type Passport = [PassportField]

data PassportField = 
    BirthYear Int
  | IssueYear Int
  | ExpirationYear Int
  | Height Units Int
  | HairColor String
  | EyeColor Color 
  | PassportId String
  | CountryId Int 
  deriving (Show)

data Units = Cm | In deriving (Show)
data Color = Amber | Blue | Brown | Gray | Green | Hazel | Other deriving (Show)

key :: PassportField -> String
key (BirthYear _)      = "byr"
key (IssueYear _)      = "iyr"
key (ExpirationYear _) = "eyr"
key (Height _ _)       = "hgt"
key (HairColor _)      = "hcl"
key (EyeColor _)       = "ecl"
key (PassportId _)     = "pid"
key (CountryId _)      = "cid"

count p = length . filter p
between n (i, j) = n >= i && n <= j

-- tries to read a value from a string, returning a `Maybe` depending on 
-- success or failure
safeRead :: Read a => String -> Maybe a
safeRead s = fst <$> (listToMaybe $ reads s)

-- `validate p v` is `Just v` if `p v`, otherwise it's nothing
validate :: (a -> Bool) -> a -> Maybe a
validate p v = if p v then Just v else Nothing

-- `validates ps v` returns `Just v` if `p v` is true for every `p` in `ps`
validates :: [(a -> Bool)] -> a -> Maybe a
validates ps v = if and $ map ($ v) ps then Just v else Nothing

parseUnits :: String -> Maybe Units
parseUnits "in" = Just In
parseUnits "cm" = Just Cm
parseUnits _    = Nothing

parseColor :: String -> Maybe Color
parseColor "amb" = Just Amber
parseColor "blu" = Just Blue
parseColor "brn" = Just Brown 
parseColor "gry" = Just Gray
parseColor "grn" = Just Green
parseColor "hzl" = Just Hazel
parseColor "oth" = Just Other 
parseColor _     = Nothing

-- parsing of fields with built-in validation
parseField :: (String, String) -> Maybe PassportField
parseField ("byr", s) = BirthYear <$> (validate (`between` (1920, 2002))  =<< safeRead s)
parseField ("iyr", s) = IssueYear <$> (validate (`between` (2010, 2020)) =<< safeRead s)
parseField ("eyr", s) = ExpirationYear <$> (validate (`between` (2020, 2030)) =<< safeRead s)
parseField ("hgt", s) = Height <$> units <*> (validateHeight units =<< (safeRead $ takeWhile isDigit s))
  where units = (parseUnits $ dropWhile isDigit s)
        validateHeight (Just In) = validate (`between` (59, 76))
        validateHeight (Just Cm) = validate (`between` (150, 193))
        validateHeight _         = const Nothing
parseField ("hcl", s) = HairColor <$> validates [(and.map isHexDigit. drop 1), ((=="#").take 1), ((==7).length)] s
parseField ("pid", s) = PassportId <$> validates [(and.map isDigit), ((==9).length)] s
parseField ("cid", s) = CountryId <$> safeRead s
parseField ("ecl", s) = EyeColor <$> (parseColor s)
parseField (_, _)     = Nothing
                                       
preprocessPassports :: [String] -> [[(String, String)]]
preprocessPassports = map (map splitPair . splitOn " " . intercalate " ") . splitWhen null
  where splitPair ps = let [k, v] = splitOn ":" ps in (k, v)

keysPresent :: [String] -> Bool
keysPresent = (== length requiredKeys) . length . intersect requiredKeys 
  where requiredKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

parsePassport :: [(String, String)] -> Passport
parsePassport = mapMaybe parseField

keys :: Passport -> [String]
keys = map key

-- count how many passports have all needed keys, without validating their values
part1 :: [String] -> Int
part1 = count keysPresent . map (map fst) . preprocessPassports

-- validate field values and count all keys are present
part2 :: [String] -> Int
part2 = count keysPresent . map (keys.parsePassport) . preprocessPassports

main = do
  input <- readFile "input.txt"
  let passports =  lines input
  print $ part1  passports
  print $ part2 passports
