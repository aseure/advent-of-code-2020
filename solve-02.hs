import Control.Applicative
import Control.Lens

main = do
    content <- readFile "input-02.txt"
    let parsedLines = map parse $ lines content
    putStrLn $ show . length $ filter isValid1 parsedLines
    putStrLn $ show . length $ filter isValid2 parsedLines

parse :: String -> (Policy, String)
parse s =
    let split = words s
        p = policy (split !! 0) (split !! 1)
    in (p, split !! 2)

data Policy = Policy Int Int Char

policy :: [Char] -> [Char] -> Policy
policy repeat char =
    let i1 = (read::String->Int) $ takeWhile (/= '-') repeat
        i2 = (read::String->Int) $ (reverse $ takeWhile (/= '-') (reverse repeat))
        letter = head char
     in Policy i1 i2 letter

isValid1 :: (Policy, String) -> Bool
isValid1 (Policy min max char, password) =
    let n = length (filter (== char) password)
    in min <= n && n <= max

isValid2 :: (Policy, String) -> Bool
isValid2 (Policy pos1 pos2 char, password) =
    let c1 = password ^? ix (pos1-1)
        c2 = password ^? ix (pos2-1)
        maybeMatch = liftA (== char)
        maybeDifferent = liftA2 (/=) (maybeMatch c1) (maybeMatch c2)
     in case maybeDifferent of
          Just True -> True
          _ -> False
