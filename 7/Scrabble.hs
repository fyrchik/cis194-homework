{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

-- Exercise 3
module Scrabble where

import Data.Char (toUpper)

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

scoreToInt (Score i) = i

instance Monoid Score where
    mempty  = 0
    mappend = (+)

score :: Char -> Score
score e | test "AEILNORSTU" = Score 1
        | test "DG" = Score 2
        | test "BCMP" = Score 3
        | test "FHVWY" = Score 4
        | test "K" = Score 5
        | test "JX" = Score 8
        | test "QZ" = Score 10
        | otherwise = Score 0
  where test = elem $ toUpper e

scoreString :: String -> Score
scoreString = mconcat . map score
