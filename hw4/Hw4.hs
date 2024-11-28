import Control.Arrow -- use >>> instead of . for proper composition
import Data.Function
import Data.List
import Data.Set qualified as Set
import Text.Printf (printf)

-- EXERCISE 1

fun1 :: [Integer] -> Integer
fun1 = filter even >>> map (2 -) >>> product

-- fun1 = product . map (2 -) . filter even

fun2 :: Integer -> Integer
fun2 = iterate seq >>> takeWhile (> 1) >>> filter even >>> sum
  where
    seq n = if even n then n `div` 2 else 3 * n + 1

-- EXERCISE 2
data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Eq)

-- compareTree :: Tree a -> Tree a -> Ordering
-- compareTree = compare `on` height

height :: Tree a -> Integer
height Leaf = -1
height (Node n _ _ _) = n

maxSubHeight :: Tree a -> Tree a -> Integer
maxSubHeight lt rt = max (height lt) (height rt)

nodeAdder :: a -> Tree a -> Tree a
nodeAdder x Leaf = Node 0 Leaf x Leaf
nodeAdder x (Node n lt node rt)
  | height lt <= height rt = Node (1 + maxSubHeight addedLt rt) addedLt node rt
  | otherwise = Node (1 + maxSubHeight lt addedRt) lt node addedRt
  where
    addedLt = nodeAdder x lt
    addedRt = nodeAdder x rt

foldTree :: [a] -> Tree a
foldTree = foldr nodeAdder Leaf

instance (Show a) => Show (Tree a) where
  show = showAtLevel 0
    where
      showAtLevel l Leaf = addSpace l ++ show "Leaf"
      showAtLevel l (Node h lt x rt) = printf "%s%s\n%s\n%s" (addSpace l) (show x) (showAtLevel (l + 1) lt) (showAtLevel (l + 1) rt)
      addSpace = flip replicate '\t'

xor :: [Bool] -> Bool
xor = foldr xorPair False
  where
    xorPair p q = not (p && q) && (p || q) -- xor instead of xorPair: Bad practice?

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (f >>> (:)) []

-- Ex: Implement foldl with foldr
-- TODO

-- Exercice 4
-- cartProd :: [a] -> [b] -> [(a, b)]
-- cartProd xs ys = [(x, y) | x <- xs, y <- ys]

sieveSundram :: Integer -> [Integer]
sieveSundram n = [1 .. n] \\ removed & map ((* 2) >>> (+ 1))
  where
    removed =
      [ k
        | j <- [1 .. n],
          i <- [1 .. j],
          let k = i + j + 2 * i * j,
          k <= n
      ]

-- Not even faster, so not worth...
-- sieveSundram :: Integer -> [Integer]
-- sieveSundram n = [1 .. n] & Set.fromList & (`Set.difference` removed) & Set.map ((* 2) >>> (+ 1)) & Set.toList
--   where
--     removed =
--       Set.fromList
--         [ i + j + 2 * i * j
--           | j <- [1 .. n],
--             i <- [1 .. j],
--             i + j + 2 * i * j < n + 1
--         ]
--     numberList n = [1 .. n]
