module Sugar1 where

import HW1


--
-- Exercise 1: Syntactic Sugar
-- 
-- Often we can add features to a language without extending its abstract
-- syntax by "translating" the new features into existing features.
--
-- Such new features are called "syntactic sugar".
--
-- Questions to think about (we'll discuss at the end of the exercise):
--
--  * What are the benefits of extending a language in this way?
--  * What are the costs?
--  * Is it always possible to add new features as syntactic sugar?
--  


-- For each of the following function specifications:
--  1. Write the type of the function.
--  2. Implement the function.


-- | Takes an expression and returns an expresion that evaluates to its
--   negation.
--
--   >>> eval e2
--   65
--
--   >>> eval (neg e2)
--   -65
--
neg :: Expr -> Expr
neg e = Mul (Lit (-1)) e



-- | Takes two expressions and returns an expression that evalautes to the
--   second expression subtracted from the first.
--
--   >>> eval e1
--   14
--
--   >>> eval (sub e2 e1)
--   51
sub :: Expr -> Expr -> Expr
sub l r = Add l (neg r)
