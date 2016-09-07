-- Exercise 1
toDigits    :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]

toDigitsRev n
    | n <= 0    = []
    | otherwise = m : toDigitsRev d
            where (d,m) = divMod n 10

toDigits = reverse . toDigitsRev

-- Exercise 2
-- TODO: rewrite with fold
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . aux . reverse
        where aux (x:y:xs) = x : (y * 2) : aux xs
              aux _        = []

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . map toDigits

-- Exercise 4
validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther . toDigits) n `mod` 10 == 0

-- Exercise 5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

hanoi n a b c
    | n == 1    = [(a,b)]
    | otherwise = hanoi (n-1) a c b ++ ((a,b) : hanoi (n-1) c b a)

-- Exercise 6 (Optional)
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n a b c d
    | n == 1    = [(a,b)]
