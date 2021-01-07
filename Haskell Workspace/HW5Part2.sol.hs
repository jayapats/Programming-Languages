module HW4Part2 where


-- 1. Revised syntax
--
-- bool ::= `true` | `false`
--
-- reg  ::= `A` | `B`
--
-- expr ::= int
--       |  reg
--       |  expr + expr
-- 
-- test ::= bool
--       |  expr <= expr
--       |  `not` test
--       |  test & test
--
-- stmt ::= reg := expr
--       |  `if` test `then` prog `else` prog `end`

data Reg = A | B
  deriving (Eq,Show)

data Expr = LitI Int
          | Get Reg
          | Add Expr Expr
  deriving (Eq,Show)

data Test = LitB Bool
          | LTE Expr Expr
          | Not Test
          | And Test Test
  deriving (Eq,Show)

data Stmt = Set Reg Expr
          | If Test Prog Prog
  deriving (Eq,Show)

type Prog = [Stmt]
