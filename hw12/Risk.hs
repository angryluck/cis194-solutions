{- cabal:
    build-depends: base, MonadRandom
 -}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List (sort, sortBy)
import Data.Ord (Down (Down), comparing)

import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.Functor ((<&>))

------------------------------------------------------------
-- Die values

newtype DieValue = DV {unDV :: Int}
    deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
    random = first DV . randomR (1, 6)
    randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Important parts of documentation
--
-- Rand g is synonym for RandT g applied to identity monad
-- type Rand g = RandT g Identity
--
-- data RandT g m
--    g: generator
--    m: inner monad
--
-- Turns Rand g a into a function g -> (a,g)
-- runRand :: Rand g a -> g -> (a, g)

------------------------------------------------------------
-- Testing
-- Generate StdGen from IO, and pass it to die
test = evalRandIO die
testArmy = Battlefield 20 30

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield {attackers :: Army, defenders :: Army} deriving (Show)

-- EX1: Instal MonadRandom (see 3 lines at top)

-- EX 2:
testBF = Battlefield 7 6

dice :: Rand StdGen [DieValue]
dice = getRandoms

-- compare :: DieValue -> DieValue -> Battlefield -> Battlefield
-- compare adie ddie (Battlefield attack defend)
--   | adie > ddie = Battlefield attack (defend - 1)
--   | otherwise = Battlefield (attack - 1) defend

sortDice :: [DieValue] -> [DieValue]
sortDice = sortBy (comparing Down)

attackThrow :: Army -> Rand StdGen [DieValue]
attackThrow n = (take k >>> sortDice) <$> dice
  where
    k = min (n - 1) 3

defendThrow :: Army -> Rand StdGen [DieValue]
defendThrow n = (take k >>> sortDice) <$> dice
  where
    k = min n 2

lostArmy :: [DieValue] -> [DieValue] -> (Army, Army)
lostArmy as ds = foldl folder (0, 0) winners
  where
    winners = zipWith compare as ds
    folder (x, y) ord
        | ord == GT = (x + 1, y)
        | otherwise = (x, y + 1)

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield as ds) = do
    aDice <- attackThrow as
    dDice <- defendThrow ds
    let (alost, dlost) = lostArmy aDice dDice
    return (Battlefield (as - alost) (ds - dlost))

-- EX 3
invade :: Battlefield -> Rand StdGen Battlefield
invade bf
    | attackers bf < 2 || defenders bf < 1 = return bf
    | otherwise = battle bf >>= invade

-- EX 4
attackWin :: Battlefield -> Int
attackWin (Battlefield as ds)
    | ds == 0 = 1
    | as == 1 = 0
    -- if not ds = 0 or as = 1, then function has been used wrongly
    | otherwise = undefined

successProb :: Battlefield -> Rand StdGen Double
successProb bf = sim & sequence <&> (sum >>> fromIntegral >>> (/ 1000))
  where
    sim = replicate 1000 (invade bf) <&> fmap attackWin
