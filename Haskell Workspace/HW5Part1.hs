module StackLang where


import Prelude hiding (Num)

--- Question 1
data Reg
     = A
     | B
  deriving (Eq,Show)

data Expr
     = X Int Int
     | Y Int Bool
     | Z Int Reg
     | Add Expr Expr
     | LessThanEq Expr Expr
     | Not Expr
     | And Expr Expr
  deriving (Eq,Show)

type Prog = [Stmt]
data Stmt
     = Copy Reg Expr
     | IfElse Prog Prog
  deriving (Eq,Show)

------ Question 2
--A := 3

--exx1 :: Reg -> Prog
--exx1 A = [Copy A Expr]
--or
--ex1 :: prog
--ex1 = [Copy A CInt]

--Getting error that cInt lacks an accompanying binding

--B := A+2
--ex2 :: Prog
--ex2 = [Copy B (Add A X Int)]
--Since X is of type Int

--if A <= B then A := A + A; else B := B + B;
--ex3 :: Prog
--ex3 = [IfElse (LessThanEq A B) (Copy A A) (Copy B B)]

--B := A+B;
--ex4 :: Prog
--ex4 = [Copy B (Add A B)]

--Question 3
-- sumFiveOrLess :: [Int] -> Prog
-- sumFiveOrLess [] = Copy A 0
-- sumFiveOrLess(x:xs) = LessThanEq x 5 (Copy A (Add x A))
