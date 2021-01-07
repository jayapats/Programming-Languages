-- | Addition expressions with naming.
module Let where


-- * Abstract syntax

type Var = String

data Exp = Lit Int
         | Add Exp Exp
         | Let Var Exp Exp
         | Ref Var
  deriving (Eq,Show)

-- let x = 2+3 in x+x  ==> 10
ex1 = Let "x" (Add (Lit 2) (Lit 3))
              (Add (Ref "x") (Ref "x"))

-- let x = 2+3 in (let y = x+4 in x+y)  ==> 14
ex2 = Let "x" (Add (Lit 2) (Lit 3))
              (Let "y" (Add (Ref "x") (Lit 4))
                       (Add (Ref "x") (Ref "y")))


-- * Environments

type Env = Var -> Maybe Int

empty :: Env
empty = \x -> Nothing

get :: Var -> Env -> Maybe Int
get x m = m x

set :: Var -> Int -> Env -> Env
set x i m = \y -> if y == x then Just i else m y

exEnv :: Env
exEnv = (set "a" 3 . set "b" 4 . set "c" 5) empty
-- \y -> if y == "a" then Just 3 else
--   (\y -> if y == "b" then Just 4 else
--      (\y -> if y == "c" then Just 5 else
--         (\x -> Nothing) y) y) y



-- * Denotational semantics

type Domain = Env -> Maybe Int

sem :: Exp -> Domain
sem (Lit i)     = \m -> Just i
sem (Add l r)   = \m -> case (sem l m, sem r m) of
                          (Just i, Just j) -> Just (i+j)
                          _ -> Nothing
sem (Let x b e) = \m -> case sem b m of
                          Just i -> sem e (set x i m)
                          Nothing -> Nothing
sem (Ref x)     = \m -> get x m
