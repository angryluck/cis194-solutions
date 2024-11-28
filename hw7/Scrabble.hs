module Scrabble where

import Control.Arrow
import Data.Char

type Score = Int

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty = 0

score :: Char -> Score
score c'
  | c `elem` ['a', 'e', 'i', 'l', 'n', 'o', 'r', 's', 't', 'u'] = 1
  | c `elem` ['d', 'g'] = 2
  | c `elem` ['b', 'c', 'm', 'p'] = 3
  | c `elem` ['f', 'h', 'v', 'w', 'y'] = 4
  | c == 'k' = 5
  | c `elem` ['j', 'x'] = 8
  | c `elem` ['q', 'z'] = 10
  | otherwise = 0
  where
    c = toLower c'

scoreString :: String -> Score
scoreString = map score >>> sum
