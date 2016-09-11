{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  0. Useful utilities PogChamp
------------------------------------------------------------

-- p1 `consP` p2
-- creates a list from results of p1 and p2 if both succeed
consP :: Parser a -> Parser [a] -> Parser [a]
consP = liftA2 (:)

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = p `consP` zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = satisfy isAlpha `consP` zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

-- Simple (1st) solution is WRONG, because it will parse "(1a)" as Integer and Identifier
--   even though there is no space between
-- We can't solve (may be just me) this problem without low-level Parser considerations,
--  because necessity of space depends on context.
-- At least 1 symbol look-ahead is needed here
-- We can try to introduce smth like parseSExprTail (2nd version)
--   which would force at least one space before every SExpr after first
--   but if would fail to parse this (a(x 1)a)
--   for example, Scheme understands this '(car(cons 1()))'
-- I am sad.
parseSAtom :: Parser SExpr
parseSAtom = fmap A $ spaces *> (fmap N posInt <|> fmap I ident)

parseSExpr :: Parser SExpr
parseSExpr = spaces *> (char '(' *> fmap Comb (oneOrMore parseSExpr) <* spaces <* char ')'
                      <|> parseSAtom) 

-- Apostrophe versions assumes that elements of list are split by at least 1 space
parseSExpr' :: Parser SExpr
parseSExpr' = spaces *> -- This is the reason i am not a designer or artist
            ( char '(' *> fmap Comb (parseSExpr' `consP` parseSExprT')
           <|> parseSAtom
            ) -- Who said codestyle? Kappa

spaces1 :: Parser String
spaces1 = oneOrMore $ satisfy isSpace

-- T for Tail
parseSExprT' :: Parser [SExpr]
parseSExprT' = oneOrMore (spaces1 *> parseSExpr') <* spaces <* char ')'

sexp1 = "(a 3a)"    -- 1st version parses this, thought must fail"
sexp2 = "(a(b c)d)" -- 2nd version fails here, though must not

sexpList = [ "5"
           , "foo3"
           , "(bar (foo) 3 5 874)"
           , "(((lambda x (lambda y (plus x y))) 3) 5)"
           , "( lots of ( spaces in ) this ( one ) )"
           ]
