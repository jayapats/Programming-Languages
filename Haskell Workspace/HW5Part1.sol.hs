module HW4Part1 where


-- 1. Abstract syntax

data Reg = A | B
  deriving (Eq,Show)

data Expr = LitI Int
          | LitB Bool
          | Get Reg
          | Add Expr Expr
          | LTE Expr Expr
          | Not Expr
          | And Expr Expr
  deriving (Eq,Show)

data Stmt = Set Reg Expr
          | If Expr Prog Prog
  deriving (Eq,Show)

type Prog = [Stmt]


-- 2. Example program
example :: Prog
example =
  [ Set A (LitI 3)
  , Set B (Add (Get A) (LitI 2))
  , If (LTE (Get A) (Get B))
       [Set A (Add (Get A) (Get A))]
       [Set B (Add (Get B) (Get B))]
  , Set B (Add (Get A) (Get B))
  ]


-- 3. Program that sums numbers less than or equal to 5 (filter in object language).
sumFiveOrLess :: [Int] -> Prog
sumFiveOrLess is = Set A (LitI 0) : map stmt is
  where
    stmt i = If (LTE (LitI i) (LitI 5)) [Set A (Add (Get A) (LitI i))] []

-- 3. Program that sums numbers less than or equal to 5 (filter in metalanguage).
sumFiveOrLess' :: [Int] -> Prog
sumFiveOrLess' is = Set A (LitI 0) : map stmt (filter (<= 5) is)
  where
    stmt i = Set A (Add (Get A) (LitI i))
