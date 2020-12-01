main = do
  content <- readFile "input-01.txt"
  let ints = map (read::String->Int) (lines content)
  let tuple = findTwoEntries ints
  putStrLn $ show . product . toList $ findTwoEntries ints
  putStrLn $ show . product . toList3 $ findThreeEntries ints

findTwoEntries :: [Int] -> (Int, Int)
findTwoEntries l = head [(x, y) | x <- l, y <- l, x+y == 2020]

findThreeEntries :: [Int] -> (Int, Int, Int)
findThreeEntries l = head [(x, y, z) | x <- l, y <- l, z <- l, x+y+z == 2020]

toList :: (Int, Int) -> [Int]
toList (a, b) = [a, b]

toList3 :: (Int, Int, Int) -> [Int]
toList3 (a, b, c) = [a, b, c]
