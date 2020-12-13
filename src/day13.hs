import Data.List.Split
import Data.List
import Data.Function

time t id = id * f t id
  where
    f a b = if a `mod` b == 0 then a `div` b else (a `div` b) + 1

-- It appears that q is always prime. If not this should be changed
modInverse p q = p^(q-2)

findSol :: [(Integer,Integer)] -> Integer
findSol ids = sum (zipWith (\(mj,i) j -> f (-i) mj j) ids [1..]) `mod` p
  where
    f r mj j = r * (p `div` mj) * modInverse (p `div` mj) mj
    p = product (map fst ids)

readIds :: String -> [(Integer,Integer)]
readIds s =
  map (\(a,i) -> (read a, i)) (filter ((/="x") . fst) (zip (splitOn "," s) [0..]))

main = do
  [a,b] <- lines <$> readFile "../inputs/day13.txt"
  let t   = read a :: Integer
      ids = readIds b
      ts  = map (((,) <*> time t) . fst) ids
      erliest = minimumBy (compare `on` snd) ts
  print (fst erliest * (snd erliest - t))
  print (findSol ids)
