{-# OPTIONS_GHC -fno-warn-missing-methods #-}

import Control.Arrow
import Data.Function

-- EX 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = [fib n | n <- [0 ..]]

-- EX 2
fibs2 :: [Integer]
fibs2 = iterate (\(x, y) -> (y, x + y)) (0, 1) & map fst

-- EX 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance (Show a) => Show (Stream a) where
  show x = (streamToList >>> take 20 >>> show >>> init) x ++ ",...]"

-- EX 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) $ streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x $ streamFromSeed f (f x)

-- EX 5
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

-- Implementation 1
ruler :: Stream Integer
ruler = nats & streamMap (+ 1) & streamMap power2
  where
    power2 n
      | even n = 1 + power2 (n `quot` 2)
      | otherwise = 0

-- ruler' and ruler'' doesn't work if we write
--  "interleaveStreams (Cons x xs) (Cons y ys)" instead, hmmm
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys =
  Cons x $ interleaveStreams ys xs

-- Implementation 2, using interleaveStreams
ruler' :: Stream Integer
ruler' = f 0
  where
    f n = interleaveStreams (streamRepeat n) $ f (n + 1)

-- Implementation 3, using interleaveStreams (from stack)
ruler'' :: Stream Integer
ruler'' = interleaveStreams (streamRepeat 0) (streamMap (+ 1) ruler'')

-- EX 6
x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate = streamMap negate
  (Cons a as) + (Cons b bs) = Cons (a + b) (as + bs)
  (Cons a as) * bb@(Cons b bs) = Cons (a * b) $ fromInteger a * bs + as * bb

instance Fractional (Stream Integer) where
  (Cons a as) / (Cons b bs) = q
    where
      q = Cons (a `div` b) $ fromInteger (1 `div` b) * (as - q * bs)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x ^ 2)

-- EX 7
type Matrix = (Integer, Integer, Integer, Integer)

instance Num Matrix where
  (a11, a12, a21, a22) * (b11, b12, b21, b22) =
    ( a11 * b11 + a12 * b21,
      a11 * b12 + a12 * b22,
      a21 * b11 + a22 * b21,
      a21 * b12 + a22 * b22
    )

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = fn
  where
    (fn, _, _, _) = (1, 1, 1, 0) ^ n :: Matrix
