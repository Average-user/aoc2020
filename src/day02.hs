import Data.Maybe

xor a b = (a || b) && (not (a && b)) 

valid :: String -> (Bool,Bool)
valid s = ( let c = length (filter (==l) p) in a <= c && c <= b
          , xor (l == p!!(a-1)) (l == p!!(b-1)))
  where
    [i, l', p] = words s
    l          = head l'
    (a,b)      = (read (takeWhile (/='-') i), read (tail (dropWhile (/='-') i)))

main = do
  lines <- lines <$> readFile "inputs/day02.txt"
  let r = map valid lines
  print (length (filter fst r))
  print (length (filter snd r))
