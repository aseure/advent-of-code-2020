main = do
    content <- readFile "input-02.txt"
    let nbValid = length $ filter isValid (map parse $ lines content)
    putStrLn $ show nbValid

parse :: String -> (Policy, String)
parse s =
    let split = words s
        p = policy (split !! 0) (split !! 1)
    in (p, split !! 2)

data Policy = Policy Int Int Char

policy :: [Char] -> [Char] -> Policy
policy repeat char =
    let min = (read::String->Int) $ takeWhile (/= '-') repeat
        max = (read::String->Int) $ (reverse $ takeWhile (/= '-') (reverse repeat))
        letter = head char
     in Policy min max letter

isValid :: (Policy, String) -> Bool
isValid (Policy min max char, password) =
    let n = length (filter (== char) password)
    in min <= n && n <= max
