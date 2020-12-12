import Data.List.Split
import qualified Data.Map as M
import Data.List
import Data.Maybe

necessary = ["byr","ecl","eyr","hcl","hgt","iyr","pid"]

getvals s = M.fromList
          $ map (\[x,y] -> (x,y))
          $ concatMap (map (splitOn ":") . words) (splitOn "\n" s)

valid2 :: M.Map String String -> Bool
valid2 p = and [ length byr == 4, r byr' 1920 2002
               , length iyr == 4, r iyr' 2010 2020
               , length eyr == 4, r eyr' 2020 2030
               , ht hgt
               , head hcl == '#', length hcl == 7,
                 all (`elem` (['0'..'9']++['a'..'f'])) (tail hcl)
               , ecl `elem` ["amb","blu","brn","gry","grn","hzl","oth"]
               , all (`elem` ['0'..'9']) pid, length pid == 9
               ]
  where
    [byr,ecl,eyr,hcl,hgt,iyr,pid] = map (p M.!) necessary
    r i a b = a <= i && i <= b
    [byr',iyr',eyr'] = map read [byr,iyr,eyr] :: [Int]
    ht s | not (null s) && (s == n ++ "cm") = r n' 150 193
         | not (null s) && (s == n ++ "in") = r n' 59 76
         | otherwise                        = False
      where
        n = takeWhile (`elem` ['0'..'9']) s
        n' = read n :: Int
    
main = do
  lines <- splitOn "\n\n" <$> readFile "inputs/day04.txt"
  let vals = map getvals lines
      valid1 = filter (\p -> (all (isJust . (M.!?) p) necessary)) vals

  print (length valid1)
  print (length (filter valid2 valid1))
