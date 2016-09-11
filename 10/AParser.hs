{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char
import           Data.Tuple (swap)
import           Data.Maybe (isNothing)

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------
-- Exercise 1
-- this is fmap for imaginable Functor (,c)
first :: (a -> b) -> (a, c) -> (b, c)
first g = swap . fmap g . swap 

-- Maybe is Monad! Am i cheating here? :)
instance Functor Parser where
    fmap g = Parser . ((>>= Just . first g) .) . runParser
--  OK, less pointfreeish
--  fmap g (Parser p) = Parser (\t -> p t >>= Just . first g)

-- Exercise 2
-- no cheating :(
instance Applicative Parser where
  pure x = Parser $ Just . (,) x
  g <*> h = Parser (\s ->
            runParser g s >>= \(f,s1) ->
            runParser h s1 >>= Just . first f)
--  (<*>) g h = Parser (\s ->
--      let rf = runParser g s in
--        case rf of
--          Nothing -> Nothing
--          Just (f,s1) ->
--            let fx = runParser h s1 in
--              case fx of
--                Nothing -> Nothing
--                Just (x,s2) -> Just (f x,s2))

-- Exercise 3
abParser :: Parser (Char, Char)
abParser = fmap (,) (char 'a') <*> char 'b'

abParser_ :: Parser ()
abParser_ = fmap (const ()) abParser

pToList :: Parser a -> Parser [a]
pToList = fmap (:[])

intPair :: Parser [Integer]
intPair =  fmap (++) (pToList posInt)
       <*> (fmap (flip const) (char ' ') <*> pToList posInt)

-- Exercise 4
-- Alternative Maybe Keepo
instance Alternative Parser where
  empty = Parser (const Nothing)
  p1 <|> p2 = Parser (\s -> runParser p1 s <|> runParser p2 s)

-- Exercise 5
forget :: Parser a -> Parser ()
forget = fmap (const ())

intOrUppercase :: Parser ()
intOrUppercase = forget posInt <|> forget (satisfy isUpper)
