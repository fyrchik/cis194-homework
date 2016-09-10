{-# LANGUAGE FlexibleInstances #-}
import Data.Monoid

import Buffer
import Editor
import Scrabble
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Exercise 1
tag :: Monoid m => JoinList m a -> m
tag (Single t _)   = t
tag (Append t _ _) = t
tag Empty          = mempty

-- Some obvious optimizations
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ y = y
x +++ Empty = x
x +++ y = Append (tag x `mappend` tag y) x y

-- Exercise 2
(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

intS :: Sized b => b -> Int
intS = getSize . size

intT :: (Sized b, Monoid b) => JoinList b a -> Int
intT = intS . tag

indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ i Empty     = Nothing
indexJ i (Single _ x)
  | i == 0    = Just x
  | otherwise = Nothing
indexJ i (Append t l r)
  | i > intS t  = Nothing
  | i < intT l = indexJ i l
  | otherwise   = indexJ (i - intT l) r

dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i jl@(Single _ _)
  | i <= 0    = jl
  | otherwise = Empty
dropJ i jl@(Append t l r)
  | i < intT l  = dropJ i l +++ r 
  | i >= intS t = Empty
  | otherwise   = dropJ (i - intT l) r

takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i jl@(Single _ _)
  | i <= 0    = Empty
  | otherwise = jl
takeJ i jl@(Append t l r)
  | i <= intT l = takeJ i l
  | i >= intS t = jl
  | otherwise   = l +++ takeJ (i - intT l) r

-- Exercise 3
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- Exercise 4
instance Buffer (JoinList (Score, Size) String) where
  toString (Single _ s) = s
  toString (Append t l r) = toString l ++ ('\n' : toString r)
  toString _ = ""

  -- no, i am not a pervert
  -- just lazy
  -- like Haskell ;)
  fromString = foldr (+++) Empty . map ssLine . lines

  line = indexJ

  -- obviously suboptimal, but i am drunk and upset :(
  replaceLine n s b = takeJ n b +++ fromString s +++ dropJ (n+1) b

  numLines = intT

  value = scoreToInt . fst . tag


ssLine s = Single (scoreString s, Size 1) s

main = runEditor editor $ ((fromString $ unlines
     [ "This buffer is for notes you don't want to save, and for"
     , "evaluation of steam valve coefficients."
     , "To load a different file, type the character L followed"
     , "by the name of the file."
     ]) :: JoinList (Score, Size) String)
