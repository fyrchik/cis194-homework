{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}

-- Exercise 1
fib :: Integer -> Integer
fib n | n == 0 = 0
      | n == 1 = 1
      | otherwise = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x s) = x : streamToList s

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x s) = Cons (f x) $ streamMap f s

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f i = Cons i $ streamFromSeed f (f i)

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

streamFoldr :: (a -> b -> b) -> Stream a -> b
streamFoldr f (Cons x s) = f x (streamFoldr f s)

ruler :: Stream Integer
ruler = streamFoldr interleaveStreams $ streamMap streamRepeat nats -- orgasm Kreygasm

-- Exercise 6 (Optional)
zeros :: Stream Integer
zeros = streamRepeat 0

x :: Stream Integer
x = Cons 0 $ Cons 1 $ zeros

streamZipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
streamZipWith f (Cons a as) (Cons b bs) =
  Cons (f a b) $ streamZipWith f as bs

instance Num (Stream Integer) where
  fromInteger n = Cons n $ zeros
  negate        = streamMap negate
  (+)           = streamZipWith (+) 
  (*) (Cons a as) c@(Cons b bs) =
    Cons (a * b) $ streamMap (*a) bs + as * c

instance Fractional (Stream Integer) where
  (/) (Cons a as) (Cons b bs) = q
    where q = Cons (div a b) $ streamMap (`div` b) (as - q * bs)

fibs3 :: Stream Integer
fibs3 = x / (Cons 1 (Cons (-1) (Cons (-1) zeros)))

-- Exercise 7 (Optional)
data Matrix a = Matrix a a a a

instance Num a => Num (Matrix a) where
  (Matrix a11 a12 a21 a22) * (Matrix b11 b12 b21 b22) =
    Matrix (a11 * b11 + a12 * b21) (a11 * b12 + a12 * b22)
           (a21 * b11 + a22 * b21) (a21 * b12 + a22 * b22)

f :: Matrix Integer
f = Matrix 1 1 1 0

get12 (Matrix _ x _ _) = x

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = get12 (f ^ n)
