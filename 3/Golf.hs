module Golf where

import Data.List (sort, group, tails)
import Data.Bool (bool)

-- Exercise 1
-- relatively short but not the fastest :)

nth :: [a] -> Int -> [a]
nth (x:xs) n = (x:) $ nth (drop (n-1) xs) n
nth _ _      = []

skips :: [a] -> [[a]]
skips [] = []
skips s  = s : zipWith nth (tail $ tails s) [2 .. length s]

-- Exercise 2

localMaxima :: [Integer] -> [Integer]
localMaxima (x:l@(y:z:xs))
  | y > max x z = y : localMaxima l
  | otherwise   = localMaxima l
localMaxima _   = []

-- Exercise 3

-- [1,2,3,1,4,9,9,9] -> [0,2,1,1,1,0,0,0,3,0]
getHist :: [Integer] -> [Integer]
getHist = map (toInteger . length . tail) . group . sort . ([0..9]++)

toString :: a -> ([Integer] ,String) -> ([Integer], String)
toString _ (l,s) = (map (\x -> x-1) l, n)
    where n = map (\x -> bool ' ' '*' (x>0)) l ++ ('\n':s)

-- ugly shit
histogram :: [Integer] -> String
histogram l = snd $ foldr toString (h,"==========\n0123456789\n")  [1 .. maximum h]
    where h = getHist l
