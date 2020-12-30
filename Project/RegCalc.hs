-- | A single-register calculator language.
module RegCalc where


-- * Abstract syntax

data Exp = Lit Int
         | Add Exp Exp
         | Save Exp
         | Load
  deriving (Eq,Show)

-- save 3 + load  ==>  6
ex1 = Add (Save (Lit 3)) Load

-- save ((save 3 + load) + load) + load  ==>  18
ex2 = Add (Save (Add ex1 Load)) Load


-- * Denotational semantics

type Reg = Int

type Domain = Reg -> (Reg, Int)

sem :: Exp -> Domain
sem = undefined
