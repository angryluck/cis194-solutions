module Golf where

import Data.List

skips :: [a] -> [[a]]
skips x = map (s x) [0 .. length x - 1]

s :: [a] -> Int -> [a] -- s for skipped
s x n = [c | (c, k) <- zip x [0 ..], mod k n == 0]

-- s x n = if null k then [] else head k : s (tail k) n
--   where
--     k = drop n

--
-- SHORTER, but complains when using "True" rather than "otherwise"
-- s x n
--   | null k = []
--   | True = head k : s (tail k) n
--   where
--     k = drop n x

-- Clean try first
-- localMaxima :: [Integer] -> [Integer]
-- localMaxima x = [b | (a, b, c) <- triples x, isMaxima a b c]
--
-- triples :: [a] -> [(a, a, a)]
-- triples x = zip3 x (drop 1 x) (drop 2 x)
--
-- isMaxima :: Integer -> Integer -> Integer -> Bool
-- isMaxima x y z = y > max x z

-- More efficient try

localMaxima :: [Integer] -> [Integer]
localMaxima x = [b | (a, b, c) <- zip3 x (drop 1 x) (drop 2 x), b > max a c]

-- List comprehension, as it's shorter than map/filter.
-- First we create a new list consisting of triples, by zipping the list,
--  the list shifted by 1, and the list shifted by 2.
-- Then we filter for those where the middle value of the triple is larger than
--  the other 3 values (b>max a c is one character fewer than b>a && b>c).
-- Then we map each triple, passing the filter, to its middle value

-- FIRST TRY!
histogram' :: [Integer] -> String
histogram' = (++ "==========\n0123456789\n") . unlines . transpose . e . d

-- Count Digits
c :: Integer -> [Integer] -> Int
c n = length . filter (n ==)

-- Convert list to list counting digits
d :: [Integer] -> [Int]
d xs = map (`c` xs) [0 .. 9]

-- Convert ints to strings of stars
e :: [Int] -> [String]
e x = map (\y -> replicate (maximum x - y) ' ' ++ replicate y '*') x

-- b :: String
-- b = "==========\n0123456789\n"

histogram :: [Integer] -> String
histogram x = (unlines . transpose . e $ map (\y -> length $ filter (y ==) x) [0 .. 9]) ++ "==========\n0123456789\n"

-- = [length $ filter (n ==) x | n <- [0 .. 9]]
