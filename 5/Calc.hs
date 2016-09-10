{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Calc where

import qualified ExprT as E
import qualified StackVM as S
import Parser (parseExp)


import qualified Data.Map.Strict as M
import Data.Bool (bool)
import Data.Maybe (maybe)
import Control.Monad (liftM2) -- dont blame me!!!

-- Exercise 1

eval :: E.ExprT -> Integer
eval (E.Lit x) = x
eval (E.Add e1 e2) = eval e1 + eval e2
eval (E.Mul e1 e2) = eval e1 * eval e2

-- Exercise 2

evalStr :: String -> Maybe Integer
evalStr = parseExp id (+) (*)

-- Exercise 3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr E.ExprT where
  lit = E.Lit
  add = E.Add
  mul = E.Mul

reify :: E.ExprT -> E.ExprT
reify = id

-- Exercise 4

-- Integer
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

-- Bool
instance Expr Bool where
  lit = bool True False . (<= 0)  -- say YES to pointfree
  add = (||)
  mul = (&&)

-- MinMax
newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

-- Mod7
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7
  add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)

-- Tests
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7

-- Exercise 5
instance Expr S.Program where
  lit x     = [ S.PushI x ]
  add p1 p2 = p1 ++ p2 ++ [S.Add]
  mul p1 p2 = p1 ++ p2 ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = parseExp lit add mul

-- Exercise 6
class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
              | Var String
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x = const (Just x)

  -- hard life without monads BibleThump
  add x y = \m -> let { s1 = x m ; s2 = y m } in
    case (s1,s2) of
      (Nothing, _) -> Nothing
      (_, Nothing) -> Nothing
      (Just a, Just b) -> Just $ a + b

  -- another way to write it
  mul x y = \m -> liftM2 (*) (x m) (y m)

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
