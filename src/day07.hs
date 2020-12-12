import Data.List.Split
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

amountbag :: T.Text -> (Int, T.Text)
amountbag s = (n, head (T.splitOn (T.pack " bag")
                        (T.dropWhile (`elem` (' ':['0'..'9'])) s')))
  where
    s' = T.strip s
    n  = read (T.unpack (T.takeWhile (`elem`['0'..'9']) s'))

canContain :: T.Text -> (T.Text, [(Int, T.Text)])
canContain s | T.isInfixOf (T.pack "no") s = (clean a, [])
             | otherwise                   = (clean a, map amountbag bs)
  where
    [a,b]   = T.splitOn (T.pack "contain") s
    bs      = T.splitOn (T.pack ", ") b
    clean s = head $ T.splitOn (T.pack " bag") (T.dropWhile (`elem` (' ':['0'..'9'])) s)

canContainShiny :: M.Map T.Text [(Int, T.Text)] -> T.Text -> Bool
canContainShiny m bag = f bag S.empty
  where
    f x seen | (T.pack "shiny gold") `S.member` seen = True
             | otherwise = any (\i -> f i ns) c
      where
        c = filter (`S.notMember` seen) (map snd (m M.! x))
        ns = foldl (\s x -> S.insert x s) seen c

countbags :: M.Map T.Text [(Int, T.Text)] -> T.Text -> Int
countbags m x = f x -1
  where
    f x | null xs     = 1
        | otherwise   = 1 + sum (map (\(i,x) -> i*(f x)) xs) 
      where
        xs = m M.! x

main = do
  ls <- T.lines <$> TIO.readFile "../inputs/day07.txt"
  let m = M.fromList (map canContain ls)
      bs = M.keys m
  print (length (S.fromList (filter (canContainShiny m) (M.keys m))))
  print (countbags m (T.pack "shiny gold"))
