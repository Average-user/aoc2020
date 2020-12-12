import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List

dirs = [(1,0),(0,1),(-1,0),(0,-1),(1,-1),(-1,1),(1,1),(-1,-1)]

next (x,y) = map (\(j,k) -> (x+j,y+k)) dirs

inDir m (x,y) (j,k) = f (x+j,y+k)
  where
    f (a,b) = case snd <$> m M.!? (a,b) of
                Nothing -> Nothing
                Just '.' -> f (a+j,b+k)
                Just v   -> Just v

newVal r f m c (_,a) | a == 'L' && o == 0  = (True,'#')
                     | a == '#' && o >= r = (True,'L')
                     | otherwise = (False,a)
  where
    ns = f m c
    o  = length (filter (=='#') ns)

findRep r f m = map snd $ head $ dropWhile (or . map fst) $ map (M.elems) (iterate next m)
  where
    next m = M.mapWithKey (newVal r f m) m

buildMap xs = M.fromList [((x,y), (True,xs!!y!!x)) | x <- [0..m-1], y <- [0..n-1]]
  where
    (n,m) = (length xs, length (head xs))

main = do
  ls <- lines <$> readFile "inputs/day11.txt"
  let m = buildMap ls
      f1 m   = mapMaybe (fmap snd . (M.!?) m ) . next
      f2 m c = mapMaybe (inDir m c) dirs
      r      = filter (=='#') $ findRep 4 f1 m
      r2     = filter (=='#') $ findRep 5 f2 m
  print $ length r
  print $ length r2
