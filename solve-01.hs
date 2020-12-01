main = do
  content <- readFile "input-01.txt"
  let ints = map (read::String->Int) (lines content)
  let entries = findEntries ints
  print $ uncurry (*) entries

findEntries :: [Int] -> (Int, Int)
findEntries l = head [(x, y) | x <- l, y <- l, x+y == 2020]
