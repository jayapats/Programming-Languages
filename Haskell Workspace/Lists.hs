module Lists where

-- Don't worry about this line. It's just hiding some functions that are
-- usually imported by default, but which I'm defining my own versions of
-- in this intro file.
import Prelude hiding (length,sum,product,map,foldr)


-------------------
-- Haskell Lists --
-------------------

-- * Haskell's built-in list and string types
--   * cons, nil, and syntactic sugar
--   * more recursive functions

-- data [] a =
--      []
--      (:) a [a]

-- The definition of String in the Haskell Prelude looks like this:
--
--   type String = [Char]


-- | Compute the length of a list.
length :: [a] -> Int
length []     = 0
length (x:xs) = 1 + length xs

-- | Compute the sum of an integer list.
sum :: [Int] -> Int
sum []     = 0
sum (x:xs) = x + sum xs

-- | Compute the product of the elements in a list.
product :: [Int] -> Int
product []     = 1
product (x:xs) = x * product xs

-- | Double all the elements in an integer list.
doubleAll :: [Int] -> [Int]
doubleAll []     = []
doubleAll (x:xs) = 2 * x : doubleAll xs

-- | Flip all of the boolean values in a boolean list.
notAll :: [Bool] -> [Bool]
notAll []     = []
notAll (x:xs) = not x : notAll xs


----------------------------
-- Higher-Order Functions --
----------------------------


-- * Map

-- | Map a function over the elements in a list.
map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs

-- | Reimplement doubleAll using map.
doubleAll' :: [Int] -> [Int]
doubleAll' = map (\x -> 2 * x)  -- map (2 *)

-- | Reimplement notAll using map.
notAll' :: [Bool] -> [Bool]
notAll' = map not


-- * Fold

-- | Fold a function over the elements in a list.
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f b []     = b
foldr f b (x:xs) = f x (foldr f b xs)

-- | Reimplement sum using foldr.
sum' :: [Int] -> Int
sum' = foldr (+) 0

-- | Reimplement product using foldr.
product' :: [Int] -> Int
product' = foldr (*) 1

-- | Reimplement length using foldr.
length' :: [a] -> Int
length' = foldr (\x n -> n + 1) 0
