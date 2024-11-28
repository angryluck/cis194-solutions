{-# LANGUAGE FlexibleInstances #-}

-- {-# LANGUAGE TypeSynonymInstances #-}

module Calc where

-- import Language.Haskell.TH (instanceD)

{-  (Program) -}
import Control.Applicative (Applicative (liftA2))
import Control.Arrow
import Data.Function
import Data.Map qualified as M
import ExprT
import Parser
import StackVM

-- import StackVM qualified as S

-- EXERCISE 1
eval :: ExprT -> Integer
eval (ExprT.Lit n) = n
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Mul x y) = eval x * eval y

-- EXERCISE 2
-- String -> Maybe ExprT -> Maybe Integer
evalStr :: String -> Maybe Integer
evalStr = parseExp ExprT.Lit ExprT.Add ExprT.Mul >>> fmap eval

-- EXERCISE 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

-- EXERCISE 4
instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit = (`mod` 7) >>> Mod7
  add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)

testExp :: (Expr a) => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer

testBool = testExp :: Maybe Bool

testMM = testExp :: Maybe MinMax

testSat = testExp :: Maybe Mod7

-- EXERCISE 5
instance Expr Program where
  -- Want "(3* -4) + 5 -> add (mul (lit 3) (lit -4)) (lit 5)
  -- -> [Add, Mul, "
  lit i = [StackVM.PushI i]
  add xs ys = xs ++ ys ++ [StackVM.Add]
  mul xs ys = xs ++ ys ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

-- EXERCISE 6
class HasVars a where
  var :: String -> a

data VarExprT
  = Lit Integer
  | Var String
  | Add VarExprT VarExprT
  | Mul VarExprT VarExprT
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = Calc.Lit
  add = Calc.Add
  mul = Calc.Mul

instance HasVars VarExprT where
  var = Calc.Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  -- add exp1 exp2 map = liftA2 (+)  (exp1 map) (exp2 map)
  lit i = const (Just i)
  add exp1 exp2 map = (+) <$> exp1 map <*> exp2 map
  mul exp1 exp2 map = (*) <$> exp1 map <*> exp2 map

withVars ::
  [(String, Integer)] ->
  ( M.Map String Integer ->
    Maybe Integer
  ) ->
  Maybe Integer
withVars vs exp = exp $ M.fromList vs
