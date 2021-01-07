module HW9 where

import Prelude hiding (and,or,not,pred,succ,fst,snd,either)

import DeBruijn
import Church


--
-- * Church-encoded lists.
--

-- ** Encoding

-- | Pretend Haskell's Int is restricted to natural numbers.
type Nat = Int

-- | A Church-encoded list is represented by a sum of encodings for the 'nil'
--   case and 'cons' case. Nil is represented by the identity function.
nil :: Exp
nil = App inL (Abs (Ref 0))

-- | A smart constructor for building a 'cons' node, encoded as a pair of the
--   head and tail of the list.
cons :: Exp -> Exp -> Exp
cons h t = App inR (app2 pair h t)

-- | Encode a Haskell list of natural numbers in lambda calculus as a
--   Church-encoded list.
encodeList :: [Nat] -> Exp
encodeList []    = nil
encodeList (h:t) = cons (num h) (encodeList t)


-- ** Sum

-- | Write a lambda calculus function that computes the sum of a
--   Church-encoded list of natural numbers.
sumList :: Exp
sumList (e:es)=  cons e (sumList es)
-- sumList = App cons inL (sumList inR)

--Other way using fixpoint
--sumListCond :: Exp
--sumListCond = abs3 if_ (nil == snd) (zero) (app2 add (fst) (App sumList snd))

--sumList :: Exp
--sumList = App fix sumListCond

-- Applying fixpoint function for recursively calling the sumList.
-- First I am pattern matching with nil. If it is equal to nil, then returning zero.
-- Else am recursively calling the sumList by adding it to the fst.

-- | For testing your lambda-calculus encoded sum function. Should return a
--   Church-encoded number.
--runSum :: [Nat] -> Exp
--runSum l = eval (App sumList (encodeList l))

-- | A function for testing your sum function. Checks to see if the lambda
--   calculus encoding returns the same value as Haskell's sum function.
--
--   >>> testSum []
--   True
--
--   >>> testSum [2,3,4]
--   True
--
--testSum :: [Nat] -> Bool
--testSum l = num (sum l) == runSum l


-- ** Map

-- | Write a lambda calculus function that maps a function (given as its
--   first argument) over a Church-encoded list (second argument).
--mapList :: Exp
--mapList f [] = zero
--mapList f (x:xs)= App (f x ) (mapList f xs)


-- | Map function tests.
--
--   >>> :{
--     eval (encodeList [3,4,5]) ==
--     eval (app2 mapList (App add two) (encodeList [1,2,3]))
--   :}
--   True
--
--   >>> :{
--     eval (encodeList [3,6,9]) ==
--     eval (app2 mapList (App mult three) (encodeList [1,2,3]))
--   :}
--   True
