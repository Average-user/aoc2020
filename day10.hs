import Data.List

t = 1 : 1 : 2 : zipWith3 (\x y z -> x+y+z) t (tail t) (drop 2 t)

main = do
  ns <- sort . map read . lines <$> readFile "inputs/day10.txt" :: IO [Int]
  let xs    = 0 : ns ++ [maximum ns + 3]
      delta = zipWith subtract xs (tail xs)
  print $ (length (filter (==1) delta) * length (filter (==3) delta))
  print $ foldl' (*) 1 $ map ((t!!) . length) $ filter ((==1) . head) $ group delta
