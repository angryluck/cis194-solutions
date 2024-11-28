module JoinList where

import Buffer
import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.Semigroup (Product, Semigroup (sconcat))
import Scrabble
import Sized

import Editor

main =
  runEditor editor initialBuffer

initialBuffer :: JoinList (Score, Size) String
initialBuffer =
  fromString
    "This buffer is for notes you don't want to save, and for\n\
    \evaluation of steam valve coefficients.\n\
    \To load a different file, type the character L followed\n\
    \by the name of the file.\n\
    \This version is made using a custom JoinList"

-- m should be monoid
data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Ex 1
tag :: (Monoid m) => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: (Monoid m) => JoinList m a -> JoinList m a -> JoinList m a
l1 +++ Empty = l1
Empty +++ l2 = l2
l1 +++ l2 = Append (tag l1 <> tag l2) l1 l2

-- Ex 2

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x : xs) !!? 0 = Just x
(x : xs) !!? i = xs !!? (i - 1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

choseBranchJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
--  Pick branch where index lies in
choseBranchJ i l
  | i < 0 || i > s = Empty
 where
  s = l & tag & size & getSize
choseBranchJ i l = case l of
  Append _ l1 l2
    | i < s1 -> l1
    | otherwise -> l2
   where
    s1 = l1 & tag & size & getSize
  _ -> l

sizeIntJ :: (Sized b, Monoid b) => JoinList b a -> Int
sizeIntJ = tag >>> size >>> getSize

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
-- Out of bounds returns nothing
indexJ i l | i < 0 || i >= sizeIntJ l = Nothing
indexJ i l = case l of
  Empty -> Nothing
  Single _ a -> Just a
  Append _ l1 l2
    | i < s1 -> indexJ i l1
    | otherwise -> indexJ (i - s1) l2
   where
    s1 = sizeIntJ l1

-- FIX
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i l | i >= sizeIntJ l = Empty
dropJ i l | i <= 0 = l
dropJ i l = case l of
  Empty -> Empty
  Single _ a -> l
  Append s l1 l2
    | i < s1 -> Append s (dropJ i l1) l2
    | otherwise -> dropJ (i - s1) l2
   where
    s1 = sizeIntJ l1
    s2 = sizeIntJ l2

-- FIX
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i l | i >= sizeIntJ l = l
takeJ i l | i <= 0 = Empty
takeJ i l = case l of
  Empty -> Empty
  Single _ a -> l
  Append s l1 l2
    | i < s1 -> takeJ i l1
    | otherwise -> Append s l1 (takeJ (i - s1) l2)
   where
    s1 = sizeIntJ l1
    s2 = sizeIntJ l2

scoreLine :: String -> JoinList Score String
scoreLine = lines >>> map (\s -> Single (scoreString s) s) >>> foldl (+++) Empty

-- Ex 4
instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ s) = s
  toString (Append _ l1 l2) = toString l1 ++ toString l2

  fromString =
    lines
      >>> map (\s -> Single (scoreString s, 1) s)
      >>> foldl (+++) Empty

  line = indexJ

  replaceLine i s l = takeJ i l +++ Single (scoreString s, 1) s +++ dropJ (i + 1) l

  numLines = tag >>> snd >>> getSize

  value = tag >>> fst

test1 :: JoinList Size Char
test1 =
  Append
    4
    ( Append
        3
        (Single 1 'y')
        ( Append
            2
            (Single 1 'e')
            (Single 1 'a')
        )
    )
    (Single 1 'h')

test2 :: JoinList (Score, Size) String
test2 =
  Append
    (23, 2)
    (Single (9, 1) "yay ")
    (Single (14, 1) "haskell !")
