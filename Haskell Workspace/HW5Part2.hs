module StackLang where


import Prelude hiding (Num)

--- Question 4
-- Seperating Int and Bools to correct the Grammer
-- int	::=	(any integer)	integers
--
-- bool	::=	true   |   false	booleans
--
-- reg	::=	A   |   B	register names
--
-- exprInt	::=	int	integer literal
--            |	reg	load from register
--            |	expr + expr	integer addition
--            |	expr <= expr	less than or equal to

--exprBool ::= bool	boolean literal
--           |	not expr	boolean negation
--           |	expr & expr	boolean conjunction
--
-- IfElse to accept only int
-- stmt	::=	reg := expr	store to register
--      |	if exprInt	conditional statement
--        then prog
--        else prog
--        end
--
-- prog	::=	ε  |  stmt ; prog	sequence of statements


--- Question 5 Encode the new abstract syntax as a set of Haskell types and data types
data Reg
     = A
     | B
  deriving (Eq,Show)

data ExprInt
     = X Int
     | Z Int Reg
     | Add ExprInt ExprInt
     | LessThanEq ExprInt ExprInt
  deriving (Eq,Show)

data ExprBool
       = Y Bool
       | Not ExprBool
       | And ExprBool ExprBool
    deriving (Eq,Show)

type Prog = [Stmt]
data Stmt
     = Copy Reg ExprInt
     | IfElse Prog Prog
  deriving (Eq,Show)
