import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List

data Inst = Nop Int | Acc Int | Jmp Int deriving (Eq,Show,Read)

cap (x:xs) = toUpper x : xs

readInst :: String -> Inst
readInst s = read (cap (filter (/='+') s))

run :: M.Map Int Inst -> (Bool,Int)
run p = f 0 0 (S.empty)
  where
    n = length p
    f i acc seen
      | i == n            = (True,acc)
      | i `S.member` seen = (False,acc)
      | otherwise = case p M.! i of
                      (Nop _) -> f (i+1) acc (S.insert i seen)
                      (Acc k) -> f (i+1) (acc+k) (S.insert i seen)
                      (Jmp k) -> f (i+k) acc (S.insert i seen)

getModVs :: M.Map Int Inst -> [M.Map Int Inst]
getModVs p = map (\i -> M.insert i (m (p M.! i)) p)
           $ filter (\i -> g (p M.! i)) [0..length p-1]
  where
    g (Nop _) = True
    g (Jmp _) = True
    g _       = False
    m (Nop x) = (Jmp x)
    m (Jmp x) = (Nop x)

main = do
  is <- map readInst . lines <$> readFile "inputs/day08.txt"
  let program = M.fromList (zip [0..] is)
  print (snd (run program))
  print (snd <$> find fst (map run (getModVs program)))
