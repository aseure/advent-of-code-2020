import Control.Applicative
import Data.Char
import Data.List
import Data.Maybe
import Text.Read
import Text.Regex.Posix

main = do
    content <- readFile "input-04.txt"
    let passports = map computePassport (groupPassportInputs $ lines content)
    putStrLn $ show (length . filter isValid1 $ passports)
    putStrLn $ show (length . filter isValid2 $ passports)

data Passport = Passport BirthYear IssueYear ExpirationYear Height HairColor EyeColor PassportID CountryID
    deriving (Show)

type BirthYear = String
type IssueYear = String
type ExpirationYear = String
type Height = String
type HairColor = String
type EyeColor = String
type PassportID = String
type CountryID = String

isValid1 :: Passport -> Bool
isValid1 (Passport byr iyr eyr hgt hcl ecl pid _) =
    foldl (\valid value  -> valid && (not $ null value)) True values
        where values = [byr, iyr, eyr, hgt, hcl, ecl, pid]

isValid2 :: Passport -> Bool
isValid2 (Passport byr iyr eyr hgt hcl ecl pid _) =
    and [ hasNDigits 4 byr
        , maybe False (>= 1920) (maybeInt byr)
        , mapOr (<= 2002) False (maybeInt byr)
        , hasNDigits 4 iyr
        , mapOr (>= 2010) False (maybeInt iyr)
        , mapOr (<= 2020) False (maybeInt iyr)
        , hasNDigits 4 eyr
        , mapOr (>= 2020) False (maybeInt eyr)
        , mapOr (<= 2030) False (maybeInt eyr)
        , checkHeight hgt
        , checkHairColor hcl
        , checkEyeColor ecl
        , hasNDigits 9 pid
        ]

mapOr :: (a -> b) -> b -> Maybe a -> b
mapOr f def m = case m of
                  Just v -> f v
                  Nothing -> def

hasNDigits :: Int -> String -> Bool
hasNDigits n s = (foldl (\b c -> b && isDigit c) True s) && (hasNChars n s)

hasNChars :: Int -> String -> Bool
hasNChars n s = length s == n

getInteger :: Maybe String -> Maybe Int
getInteger input = case input of
                     Just s -> readMaybe s
                     Nothing -> Nothing

between :: Int -> Int -> Maybe Int -> Maybe Bool
between a b = fmap (\x -> a <= x && x <= b)

checkHeight :: String -> Bool
checkHeight s
    | isSuffixOf "cm" s = (150 <= h && h <= 193)
    | isSuffixOf "in" s = (59 <= h && h <= 76)
    | otherwise = False
    where h = fromMaybe 0 (getPrefixNumber s)

checkHairColor :: String -> Bool
checkHairColor s = s =~ "#[0-9a-f]{6}"

checkEyeColor :: String -> Bool
checkEyeColor s = s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy separator = foldr f [[]]
    where f current accumulator@(x:xs)
            | current == separator = []:accumulator
            | otherwise  = (current:x):xs

groupPassportInputs :: [String] -> [[String]]
groupPassportInputs lines =
    let groupedLines = splitBy "" lines
     in map (concat . map (splitBy ' ')) groupedLines

computePassport :: [String] -> Passport
computePassport = foldl f (Passport "" "" "" "" "" "" "" "")
    where f (Passport byr iyr eyr hgt hcl ecl pid cid) kv
            | "byr:" `isPrefixOf` kv = Passport (getValue kv) iyr eyr hgt hcl ecl pid cid
            | "iyr:" `isPrefixOf` kv = Passport byr (getValue kv) eyr hgt hcl ecl pid cid
            | "eyr:" `isPrefixOf` kv = Passport byr iyr (getValue kv) hgt hcl ecl pid cid
            | "hgt:" `isPrefixOf` kv = Passport byr iyr eyr (getValue kv) hcl ecl pid cid
            | "hcl:" `isPrefixOf` kv = Passport byr iyr eyr hgt (getValue kv) ecl pid cid
            | "ecl:" `isPrefixOf` kv = Passport byr iyr eyr hgt hcl (getValue kv) pid cid
            | "pid:" `isPrefixOf` kv = Passport byr iyr eyr hgt hcl ecl (getValue kv) cid
            | "cid:" `isPrefixOf` kv = Passport byr iyr eyr hgt hcl ecl pid (getValue kv)

getKey :: String -> String
getKey = takeWhile (/= ':')

getValue :: String -> String
getValue = tail . dropWhile (/= ':')

maybeInt :: String -> Maybe Int
maybeInt = readMaybe

getPrefixNumber :: String -> Maybe Int
getPrefixNumber s = maybeInt $ takeWhile isDigit s
