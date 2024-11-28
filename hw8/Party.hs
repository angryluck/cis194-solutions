module Party where

import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Tree
import Employee

-- EX 1
-- 1.1

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps fun) = GL (emp : emps) (empFun emp + fun)

-- 1.2
instance Semigroup GuestList where
  (GL emps1 fun1) <> (GL emps2 fun2) = GL (emps1 ++ emps2) (fun1 + fun2)

instance Monoid GuestList where
  mempty = GL [] 0

-- 1.3
moreFun :: GuestList -> GuestList -> GuestList
-- Ord is defined for GuestList, soo
moreFun = max

-- EX 2
treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold f acc tree = rootLabel tree `f` foldl (treeFold f) acc (subForest tree)

-- REMOVE THIS
instance Semigroup Employee where
  emp1 <> emp2 = Emp {empName = name, empFun = fun}
    where
      name = empName emp1 ++ empName emp2
      fun = empFun emp1 + empFun emp2

-- EX 3
flipTuple :: (a, a) -> (a, a)
flipTuple (x, y) = (y, x)

maxTuple :: (Ord a) => (a, a) -> a
maxTuple (x, y) = max x y

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
-- First GL: Best list with bosses, second list: Best list without bosses
-- nextLevel bob [] = (GL [bob] (empFun bob), mempty)
nextLevel bob = foldl (<>) (mempty, GL [bob] (empFun bob)) >>> flipTuple

-- EX 4
maxFunPair :: Tree Employee -> (GuestList, GuestList)
maxFunPair tree = nextLevel (rootLabel tree) $ map maxFunPair (subForest tree)

maxFun :: Tree Employee -> GuestList
maxFun = maxFunPair >>> maxTuple

-- Try to do EX 4 with a fold instead

-- EX 5
main = readFile "company" >>= (read >>> maxFun >>> formatGuestList >>> putStr)

-- More classical way
-- main = do
--   text <- readFile "company.txt"
--   let strFunGL = text & read & maxFun & formatGuestList
--   putStr strFunGL

--
formatGuestList :: GuestList -> String
formatGuestList (GL emps fun) = firstLine : empNameList & unlines
  where
    firstLine = "Total fun: " ++ show fun
    empNameList = map empName emps
