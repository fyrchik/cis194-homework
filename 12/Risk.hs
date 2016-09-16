{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random

import Control.Monad (replicateM, liftM2)
import Data.Bool     (bool)
import Data.List     (sortBy)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

-- Probably not the shortest solution :)
-- Exercise 2
action :: Army -> Army -> Rand StdGen [DieValue]
action n m = replicateM (min n m) die >>= return . sortBy (flip compare)

attack :: Battlefield -> Rand StdGen [DieValue]
attack b = action (attackers b - 1) 3

defend :: Battlefield -> Rand StdGen [DieValue]
defend b = action (defenders b) 2

battle :: Battlefield -> Rand StdGen Battlefield
battle b = liftM2 ((span id .) . zipWith (>)) (attack b) (defend b) >>= \(x,y) ->
           return (length x, length y) >>= \(m,n) ->
          return $ Battlefield { attackers = attackers b - m, defenders = defenders b - n }

-- Exercise 3
check :: Battlefield -> Rand StdGen Bool
check b = return $ attackers b >= 2 || defenders b >= 1

invade :: Battlefield -> Rand StdGen Battlefield
invade b = check b >>= bool (battle b >>= invade) (return b)

-- Exercise 4
successProb :: Battlefield -> Rand StdGen Double
successProb b = replicateM 1000 (invade b) >>=
                return . length . filter ((<1) . defenders) >>= \w ->
                return $ fromIntegral w / 1000
