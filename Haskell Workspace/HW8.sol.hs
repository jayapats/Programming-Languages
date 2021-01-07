module HW8 where

import Prelude hiding (and,or,not,pred,succ,fst,snd,either)

import DeBruijn
import Church


--
-- * Part 1: Nameless lambda calculus
--

-- | λx. (λx.x) x
--
--   >>> eval ex1
--   Abs (Ref 0)
--
ex1 :: Exp
ex1 = Abs (App (Abs (Ref 0)) (Ref 0))

-- | (λxy.xz) z z
--
--   >>> eval ex2
--   App (Ref 0) (Ref 0)
--
ex2 :: Exp
ex2 = app2 (abs2 (App (Ref 1) (Ref 2))) (Ref 0) (Ref 0)

-- | λx. (λxy.yx) x z
--
--   >>> eval ex3
--   Abs (App (Ref 1) (Ref 0))
--   
ex3 :: Exp
ex3 = Abs (app2 (abs2 (App (Ref 0) (Ref 1))) (Ref 0) (Ref 1))


-- | Is the given nameless lambda calculus term a closed expression? That is,
--   does it contain no free variables?
--
--   >>> closed (Ref 0)
--   False
--
--   >>> closed (Abs (Ref 0))
--   True
--
--   >>> closed (Abs (App (Ref 0) (Ref 1)))
--   False
--
--   >>> closed (Abs (App (Abs (App (Ref 0) (Ref 1))) (Ref 0)))
--   True
--
closed :: Exp -> Bool
closed = check 0
  where
    check d (Ref n)   = n < d
    check d (Abs e)   = check (d+1) e
    check d (App l r) = check d l && check d r


--
-- * Part 2: Church pair update functions
--

-- | Write a lambda calculus function that replaces the first element in a
--   Church-encoded pair. The first argument to the function is the original
--   pair, the second is the new first element.
--
--   >>> :{
--     eval (app2 pair true (num 3)) ==
--     eval (app2 setFst (app2 pair (num 2) (num 3)) true)
--   :}
--   True
--
setFst :: Exp
setFst = abs2 (app2 pair (Ref 0) (App snd (Ref 1)))

-- | Write a lambda calculus function that replaces the second element in a
--   Church-encoded pair. The first argument to the function is the original
--   pair, the second is the new second element.
--
--   >>> :{
--     eval (app2 pair (num 2) true) ==
--     eval (app2 setSnd (app2 pair (num 2) (num 3)) true)
--   :}
--   True
--
setSnd :: Exp
setSnd = abs2 (app2 pair (App fst (Ref 1)) (Ref 0))


--
-- * Part 3: Church encoding a Haskell program
--

-- | Pretend Haskell's Int is restricted to Nats.
type Nat = Int

-- | A simple data type with three cases.
data Foo = N Nat | B Bool | P Nat Bool
  deriving (Eq,Show)

-- | Compute a numeric value from a Foo.
--   (This is just an arbitrary function.)
bar :: Foo -> Nat
bar (N n)     = n * 3
bar (B True)  = 1
bar (B False) = 0
bar (P n b)   = n + if b then 1 else 0

-- | Write a Haskell function that converts a Foo into a
--   lambda calculus term.
encodeFoo :: Foo -> Exp
encodeFoo (N n)   = App in13 (num n)
encodeFoo (B b)   = App in23 (bool b)
encodeFoo (P n b) = App in33 (app2 pair (num n) (bool b))

-- | Convert a Bool to a lambda calculus term.
bool :: Bool -> Exp
bool True  = true
bool False = false

-- | Write the bar function as a lambda calculus term.
barExp :: Exp
barExp = Abs (app4 case3
                   (Abs (app2 mult (Ref 0) three))
                   (Abs (app3 if_ (Ref 0) one zero))
                   (Abs (app2 add (App fst (Ref 0))
                                  (app3 if_ (App snd (Ref 0)) one zero)))
                   (Ref 0))

-- | Run your encoded bar function on an encoded Foo value.
runBar :: Foo -> Exp
runBar x = eval (App barExp (encodeFoo x))

-- | A function for testing encodeFoo and barExp. Checks to see if the lambda
--   calculus encoding returns the same number as the given value function.
--
--   >>> testFooBar (N 4)
--   True
--
--   >>> testFooBar (B True)
--   True
--
--   >>> testFooBar (B False)
--   True
--
--   >>> testFooBar (P 5 True)
--   True
--
--   >>> testFooBar (P 5 False)
--   True
--
testFooBar :: Foo -> Bool
testFooBar x = num (bar x) == runBar x
