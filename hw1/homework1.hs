{-# OPTIONS_GHC -Wall #-}

-- Exercise 1
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- Exercise 2
doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft [] = []
doubleEveryOtherFromLeft [x] = [x]
doubleEveryOtherFromLeft (x : y : zs) = x : 2 * y : doubleEveryOtherFromLeft zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherFromLeft . reverse

-- Exercise 3

sumDigits :: [Integer] -> Integer
-- sumDigits [] = 0
-- sumDigits (x : xs) = sum (toDigits x) + sumDigits xs
-- Warnings suggested this instead
sumDigits = foldr ((+) . sum . toDigits) 0

-- Exercise 4
creditCardChecksum :: Integer -> Integer
creditCardChecksum = sumDigits . doubleEveryOther . toDigits

validate :: Integer -> Bool
validate n = creditCardChecksum n `mod` 10 == 0

-- Exercise 5
type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n - 1) a c b ++ (a, b) : hanoi (n - 1) c b a

roundedSqrt :: Integer -> Integer
roundedSqrt = round . (sqrt :: Double -> Double) . fromInteger

optimalK :: Integer -> Integer
optimalK n = max (n - roundedSqrt (2 * n + 1) + 1) 1

hanoiFourPins :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoiFourPins 0 _ _ _ _ = []
hanoiFourPins 1 a _ _ d = [(a, d)]
-- hanoiFourPins 2 a b _ d = [(a, b), (a, d), (b, d)]
-- hanoiFourPins 3 a b c d = [(a, b), (a, c), (a, d), (c, d), (b, d)]
hanoiFourPins n a b c d =
  hanoiFourPins (optimalK n) a c d b
    ++ hanoi (n - optimalK n) a d c
    ++ hanoiFourPins (optimalK n) b a c d

-- hanoiFourPins n a b c d = hanoi ()
--
