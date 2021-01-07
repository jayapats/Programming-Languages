module Bool where


-- | Syntax of the simple Boolean expression language.
data Exp
   = Tru
   | Fls
   | Not Exp
   | If Exp Exp Exp
  deriving (Eq,Show)

-- | Big-step semantics.
eval :: Exp -> Exp
eval Tru        = Tru
eval Fls        = Fls
eval (Not e)    = case eval e of
                    Tru -> Fls
                    Fls -> Tru
                    e' -> error ("in eval: not a value: " ++ show e')
eval (If c t e) = case eval c of
                    Tru -> eval t
                    Fls -> eval e
                    e' -> error ("in eval: not a value: " ++ show e')

-- | One-step reduction relation for the small-step semantics.
step :: Exp -> Exp
-- reflexive cases
step Tru          = Tru
step Fls          = Fls
-- reduction rules
step (Not Tru)    = Fls
step (Not Fls)    = Tru
step (If Tru t e) = t
step (If Fls t e) = e
-- congruence rules
step (Not e)      = Not (step e)
step (If c t e)   = If (step c) t e

-- | Reflexive, transitive closure of the one-step reduction.
steps :: Exp -> Exp
-- reflexive cases
steps Tru = Tru
steps Fls = Fls
-- transitive case
steps e   = steps (step e)
-- steps e = let e' = step e in if e == e' then e else steps e'

-- | Produce the reduction sequence for an expression.
redSeq :: Exp -> [Exp]
redSeq Tru = [Tru]
redSeq Fls = [Fls]
redSeq e   = e : redSeq (step e)
