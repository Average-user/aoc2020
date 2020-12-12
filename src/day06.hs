import Data.List.Split
import qualified Data.Set as S

main = do
  gps <- splitOn "\n\n" <$> readFile "inputs/day06.txt"
  let awnsers    = S.fromList . filter (/='\n')
      awnsers2 g = foldl1 S.intersection (map S.fromList (lines g))
  print (sum (map (length . awnsers) gps))
  print (sum (map (length . awnsers2) gps)) 
