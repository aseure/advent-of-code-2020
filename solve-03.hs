main = do
    content <- readFile "input-03.txt"
    let area = parseArea $ lines content
    let strategies = [ Strategy (+1) (+1)
                     , Strategy (+3) (+1)
                     , Strategy (+5) (+1)
                     , Strategy (+7) (+1)
                     , Strategy (+1) (+2)
                     ]
    putStrLn $ show (nbTrees area $ strategies!!1)
    putStrLn $ show (product $ map (nbTrees area) strategies)

data Area = Area Int Int [Bool]
data Location = Location Int Int
data Strategy = Strategy (Int -> Int) (Int -> Int)

parseArea :: [String] -> Area
parseArea lines =
    let width = length (lines!!0)
        height = length lines
        array = map (\c -> c == '#') (concat lines)
     in Area width height array

getLocations :: Location -> Strategy -> [Location]
getLocations start strategy =
    let (Location x y) = start
        (Strategy fx fy) = strategy
        next = Location (fx x) (fy y)
     in [start] ++ getLocations next strategy

getTrees :: Area -> [Location] -> [Bool]
getTrees area (loc:locs)
    | height <= y = []
    | otherwise = [isATree area loc] ++ getTrees area locs
    where (Area _ height _) = area
          (Location _ y) = loc

isATree :: Area -> Location -> Bool
isATree (Area width height array) (Location x y)
    | height <= y = False
    | otherwise = array!!index
    where index = y * width + (x `mod` width)

nbTrees :: Area -> Strategy -> Int
nbTrees area strategy =
    let start = Location 0 0
        path = getTrees area (getLocations start strategy)
     in length $ filter (== True) path
