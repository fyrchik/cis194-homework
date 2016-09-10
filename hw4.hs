-- Exercise 1

import Data.Bool (bool)
import Data.List (product, iterate, sort)

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' = foldr (\x y -> bool 1 (x-2) (even x) * y) 1

-- less optimal, but looking more beautiful:
-- fun1' = product . map (\x -> x-2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate (\x -> bool (3*x+1)  (x `div` 2) (even x))

-- Exercise 2

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf           = -1
height (Node x _ _ _) = x

-- can be done with two guards but i like symmetry Kappa
insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node h l e r)
  | height l < height r = Node h (insert x l) e r
  | height l > height r = Node h l e (insert x r)
  | otherwise           = let n = insert x l in Node (1 + height n) n e r

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

-- Exercise 3

xor :: [Bool] -> Bool
xor = foldr (bool id not) False -- LOL

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f ini xs = foldr (flip (.) . (flip f)) id xs ini -- Say NO to lambda!

-- Exercise 4
-- TODO: remove ugly shit, write beautiful shit

tupleF :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
tupleF f g (x,y) = (f x, g y)

mergeS :: [Integer] -> [Integer] -> [Integer]
mergeS [] _ = []
mergeS x [] = x
mergeS a@(x:xs) b@(y:ys)
  | x < y     = x : mergeS xs b
  | x > y     = mergeS a ys
  | otherwise = mergeS xs b

-- not the fastest way, but i use list comprehension!
sieveSundaram :: Integer -> [Integer] -- ugly
sieveSundaram = (2:)
              . map ((+1) . (*2))
              . uncurry mergeS
              . tupleF id sort
              . (\x -> (x, [i+j+2*i*j | i <- x, j <- x]))
              . (\n -> [1..n])
