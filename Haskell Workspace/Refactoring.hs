module Refactoring where

import Prelude hiding (Maybe(..), maybe, intersperse)


--
-- * Systematic generalization
--

-- | Insert the string ", " between words in a list of words.
commas :: [String] -> [String]
commas []     = []
commas [w]    = [w]
commas (w:ws) = w : ", " : commas ws

-- | Insert the string "; " between words in a list of words.
semis :: [String] -> [String]
semis []     = []
semis [w]    = [w]
semis (w:ws) = w : "; " : semis ws

-- ** Introduce parameters for constants

seps :: String -> [String] -> [String]
seps s []     = []
seps s [w]    = [w]
seps s (w:ws) = w : s : seps s ws

commas' = seps ", "
semis' = seps "; "


-- ** Broaden the types

intersperse :: a -> [a] -> [a]
intersperse s []     = []
intersperse s [w]    = [w]
intersperse s (w:ws) = w : s : intersperse s ws

commas'' = intersperse ", "
semis'' = intersperse "; "
zeros = intersperse 0


--
-- * Abstracting repeated programming patterns
--

data Maybe a = Nothing | Just a
  deriving (Eq,Show)


-- ** Inside a calculator app

-- | Convert a floating point result to a string,
--   or return "ERROR" if on an error (Nothing).
showResult :: Maybe Float -> String
showResult Nothing  = "ERROR"
showResult (Just x) = show x

-- | Add a value to another value that may have failed. 
--   Interpret an error (Nothing) as zero.
safeAdd :: Int -> Maybe Int -> Int
safeAdd x Nothing  = x
safeAdd x (Just y) = x + y


-- ** Inside a robot app

-- | Directions.
data Dir = N | S | E | W
  deriving (Eq,Show)

-- | Robot commands.
data Command = Move Dir
             | FireLaser Dir
             | SelfDestruct
             | Stay
  deriving (Eq,Show)

-- | Move in some direction, or stay if no direction is provided.
moveCommand :: Maybe Dir -> Command
moveCommand Nothing  = Stay
moveCommand (Just d) = Move d


-- ** Abstract out common pattern

maybe :: b -> (a -> b) -> Maybe a -> b
maybe d f Nothing  = d
maybe d f (Just x) = f x

showResult' :: Maybe Int -> String
showResult' = maybe "ERROR" show

safeAdd' x = maybe x (\y -> x + y)
moveCommand' = maybe Stay Move


--
-- * Refactoring Data Types
--

-- | Abstract syntax of arithmetic expressions.
data Expr
   = Lit Int         -- ^ Literal integers
   | Add Expr Expr   -- ^ Addition expressions
   | Sub Expr Expr   -- ^ Subtraction expressions
   | Mul Expr Expr   -- ^ Multiplication expressions
  deriving (Eq,Show)

-- | Get the leftmost literal in a expression.
leftLit :: Expr -> Int
leftLit (Lit i)   = i
leftLit (Add l _) = leftLit l
leftLit (Sub l _) = leftLit l
leftLit (Mul l _) = leftLit l

-- | Get the rightmost literal in a expression.
rightLit :: Expr -> Int
rightLit (Lit i)   = i
rightLit (Add _ r) = rightLit r
rightLit (Sub _ r) = rightLit r
rightLit (Mul _ r) = rightLit r

-- | Get a list of all literals in an expression.
allLits :: Expr -> [Int]
allLits (Lit i)   = [i]
allLits (Add l r) = allLits l ++ allLits r
allLits (Sub l r) = allLits l ++ allLits r
allLits (Mul l r) = allLits l ++ allLits r

-- | Evaluate an expression.
eval :: Expr -> Int
eval (Lit i)   = i
eval (Add l r) = eval l + eval r
eval (Sub l r) = eval l - eval r
eval (Mul l r) = eval l * eval r


-- ** Factor out shared structure

data Expr'
   = Lit' Int
   | Bin' Op' Expr' Expr'
  deriving (Eq,Show)

data Op' = Add' | Sub' | Mul'
  deriving (Eq,Show)

lit :: Int -> Expr'
lit = Lit'

add, sub, mul :: Expr' -> Expr' -> Expr'
add = Bin' Add'
sub = Bin' Sub'
mul = Bin' Mul'

leftLit' :: Expr' -> Int
leftLit' (Lit' i)     = i
leftLit' (Bin' _ l _) = leftLit' l

rightLit' :: Expr' -> Int
rightLit' (Lit' i)     = i
rightLit' (Bin' _ _ r) = rightLit' r

allLits' :: Expr' -> [Int]
allLits' (Lit' i) = [i]
allLits' (Bin' _ l r) = allLits' l ++ allLits' r

eval' :: Expr' -> Int
eval' (Lit' i)        = i
eval' (Bin' Add' l r) = eval' l + eval' r
eval' (Bin' Sub' l r) = eval' l - eval' r
eval' (Bin' Mul' l r) = eval' l * eval' r

eval'' :: Expr' -> Int
eval'' (Lit' i)     = i
eval'' (Bin' o l r) = op o (eval'' l) (eval'' r)
  where
    op Add' = (+)
    op Sub' = (-)
    op Mul' = (*)
