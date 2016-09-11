module Party where

import Data.Tree
import Data.Tuple (swap)

import Employee

-- Exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons e (GL l f) = GL (e : l) (empFun e + f)

instance Monoid GuestList where
    mempty  = GL [] 0
    mappend x@(GL l1 f1) y@(GL l2 f2) = GL (l1 ++ l2) (f1 + f2)

moreFun  :: GuestList -> GuestList -> GuestList
moreFun x@(GL _ f1) y@(GL _ f2)
  | f1 < f2   = y
  | otherwise = x

-- Exercise 2
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node root ch) = f root $ map (treeFold f) ch

-- Exercise 3
-- smth like 'liftTuple id (glCons e) . swap . mconcat' is prettier
-- but where is my liftTuple ;(
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e = (\(x,y) -> (y, glCons e x)) . mconcat

-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

-- Exercise 5
-- without 'do'
--
-- Total fun: 12345
-- Evgesha Evgeniev
-- ...
--
-- smth like strformat will be better tho
printGL :: GuestList -> IO ()
printGL (GL es f)
    = putStrLn ("Total fun: " ++ show f)
    >> foldr (\e rest -> putStrLn (empName e) >> rest) (putStr "") es

main :: IO ()
main = readFile "company.txt"
    >>= printGL . maxFun . read 
