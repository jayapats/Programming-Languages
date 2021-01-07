module HW4 where

import Prelude hiding (Enum(..), sum)


--
-- * Natural numbers
--

-- | The natural numbers.
data Nat
   = Zero
   | Succ Nat
  deriving (Eq,Show)

-- | The number 1.
one :: Nat
one = Succ Zero

-- | The number 2.
two :: Nat
two = Succ one

-- | The number 3.
three :: Nat
three = Succ two

-- | The number 4.
four :: Nat
four = Succ three


-- | The predecessor of a natural number.
--
--   >>> pred Zero
--   Zero
--
--   >>> pred three
--   Succ (Succ Zero)
--
pred :: Nat -> Nat
pred Zero = Zero
pred (Succ n) = n

-- | True if the given value is zero.
--
--   >>> isZero Zero
--   True
--
--   >>> isZero two
--   False
--
isZero :: Nat -> Bool
isZero Zero = True
isZero _ = False


-- | Convert a natural number to an integer. NOTE: We use this function in
--   tests, but you should not use it in your other definitions!
--
--   >>> toInt Zero
--   0
--
--   >>> toInt three
--   3
--
toInt :: Nat -> Int
toInt Zero = 0
toInt a    = 1 + toInt(pred(a))

-- | Add two natural numbers.

--
--   >>> add one two
--   Succ (Succ (Succ Zero))
--
--   >>> add Zero one == one
--   True
--
--   >>> add two two == four
--   True
--
--   >>> add two three == add three two
--   True
--
add :: Nat -> Nat -> Nat
add Zero n = n
add n Zero     = n
add (Succ x) y = Succ (add x y)



-- | Subtract the second natural number from the first. Return zero
--   if the second number is bigger.
--
--   >>> sub two one
--   Succ Zero
--
--   >>> sub three one
--   Succ (Succ Zero)
--
--   >>> sub one one
--   Zero
--
--   >>> sub one three
--   Zero
--
sub :: Nat -> Nat -> Nat
sub Zero n = Zero
sub n Zero = n
sub (Succ m) (Succ n) = sub m n


-- | Is the left value greater than the right?
--
--   >>> gt one two
--   False
--
--   >>> gt two one
--   True
--
--   >>> gt two two
--   False
--
gt :: Nat -> Nat -> Bool
gt x y  | sub x y == Zero = False
        | otherwise = True


-- | Multiply two natural numbers.
--
--   >>> mult two Zero
--   Zero
--
--   >>> mult Zero three
--   Zero
--
--   >>> toInt (mult two three)
--   6
--
--   >>> toInt (mult three three)
--   9
--
mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ m) n = add n (mult m n)


-- | Compute the sum of a list of natural numbers.
--
--   >>> sum []
--   Zero
--
--   >>> sum [one,Zero,two]
--   Succ (Succ (Succ Zero))
--
--   >>> toInt (sum [one,two,three])
--   6
--
sum :: [Nat] -> Nat
sum [] = Zero
sum (x:xs) = add x (sum xs)


-- | An infinite list of all of the *odd* natural numbers, in order.
--
--   >>> map toInt (take 5 odds)
--   [1,3,5,7,9]
--
--   >>> toInt (sum (take 100 odds))
--   10000
--
odds = undefined
