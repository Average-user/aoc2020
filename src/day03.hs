import qualified Data.Set as S

buildSet n m lines = S.fromList [(x,y) | x <- [0..m-1], y <- [0..n-1], lines!!y!!x == '#']

main = do
  lines <- lines <$> readFile "../inputs/day03.txt"
  let (m,n)   = (length (head lines), length lines)
      ts      = buildSet n m lines
      f dx dy = filter (`S.member` ts)
              $ takeWhile (\(_,y) -> y < n)
              $ iterate (\(x,y) -> ((x + dx) `mod` m, y + dy)) (0,0)
  print (length (f 3 1))
  print $ product (map (length . uncurry f) [(1,1),(3,1),(5,1),(7,1),(1,2)])
