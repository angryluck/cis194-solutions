{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import Control.Applicative
import Control.Arrow ((>>>))
import Control.Monad (void)
import Data.Char
import Data.Function ((&))

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
 where
  f [] = Nothing -- fail on the empty input
  f (x : xs) -- check if x satisfies the predicate
  -- if so, return x along with the remainder
  -- of the input (that is, xs)
    | p x = Just (x, xs)
    | otherwise = Nothing -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

\*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
\*Parser> runParser (satisfy isUpper) "abc"
Nothing
\*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
 where
  f xs
    | null ns = Nothing
    | otherwise = Just (read ns, rest)
   where
    (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------
-- EX 1
first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser (p >>> fmap (first f))

-- EX 2
instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser (\s -> Just (a, s))

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  -- Should refactor this, somehow...
  Parser pf <*> Parser pa = Parser pb
   where
    pb s = case pf s of
      Nothing -> Nothing
      Just (f, s') -> case pa s' of
        Nothing -> Nothing
        Just (a, s'') -> Just (f a, s'')

-- EX 3
ignoreParser :: Parser a -> Parser ()
-- equiv: ignoreParser = fmap (const ())
ignoreParser = void

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = void abParser

intPair :: Parser [Integer]
intPair = (\n1 n2 -> [n1, n2]) <$> posInt <* char ' ' <*> posInt

-- EX 4
instance Alternative Parser where
  empty :: Parser a
  empty = Parser (const Nothing)

  (<|>) :: Parser a -> Parser a -> Parser a
  -- Alternate (from AParser-cis194.hs)
  -- Parser p1 <|> Parser p2 = Parser $ liftA2 (<|>) p1 p2
  Parser p1 <|> Parser p2 = Parser p3
   where
    p3 s = p1 s <|> p2 s

-- EX 5
intOrUppercase :: Parser ()
-- Doesnt work:
-- intOrUppercase = void $ posInt <|> satisfy isUpper
intOrUppercase = void posInt <|> void (satisfy isUpper)
