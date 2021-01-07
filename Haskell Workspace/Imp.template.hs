-- This module defines Imp, a simple imperative programming language.
module Imp where


--
-- * Syntax
--

-- The syntax of Imp is given by the following grammar. Tokens surrounded by
-- `backticks` are terminal symbols, other tokens are non-terminals. Sequences
-- of nonterminals are indicated by a star, e.g. stmt* is a sequence of
-- statements.
--
--     int  ::= (any integer)
--
--     var  ::= (any variable name)
--
--     expr ::= int                      literal integers
--           |  `-` expr                 integer negation
--           |  expr `+` expr            integer addition
--           |  var                      variable reference
--
--     test ::= expr `≤` expr            integer comparison
--           |  `!` test                 boolean negation
--           |  test `&&` test           boolean conjunction
--
--     stmt ::= var `:=` expr            variable assignment
--           |  `if` test `then` stmt    conditional statement
--              `else` stmt
--           |  `while` test `do` stmt   while loop
--           |  `begin` stmt* `end`      statement block
--
--     prog ::= `vars` var* `;` stmt     program
--


-- ** Abstract syntax

-- 1. Implement the abstract syntax of Imp as a set of Haskell data types.



-- ** Example program

-- An Imp program consists of a list of variable declarations and a statement
-- to execute. All variables used in the program *must* be declared at the top.
-- Usually the top-level statement will be a block, but this is not required.
--
-- Here is an example of an Imp program. The program declares two variables,
-- a and b. The statement to execute is a block that first sets a to 1071,
-- then sets b to 462, and finally executes a loop that implements Euclid's
-- algorithm to compute their greatest common divisor, which is 21.
--
--     vars a b;
--     begin
--       a := 1071
--       b := 462
--       while !(a ≤ b && b ≤ a) do
--         if a ≤ b then
--           b := b + -a
--         else
--           a := a + -b
--     end

-- 2. Encode the example program as the following Haskell value.


-- | An example Imp program.
euclid :: Prog
euclid = undefined


--
-- * Static semantics
--

-- It is a *static* error if a variable is used in a program without being
-- declared. That is, we can check whether this property is true *before*
-- running by the program by traversing the AST and checking every
-- variable reference and every assignment statement.

-- 3. Implement the following function checkProg to check this property.
--    You'll need to implement a helper function for each syntactic category.
--    I provided their type signatures for you.


-- | Statically check that all variables referenced in the program are declared
--   at the top.
checkProg :: Prog -> Bool
checkProg = undefined

-- | Statically check a statement.
checkStmt :: [Var] -> Stmt -> Bool
checkStmt = undefined

-- | Statically check a boolean expression.
checkTest :: [Var] -> Test -> Bool
checkTest = undefined

-- | Statically check an integer expression.
checkExpr :: [Var] -> Expr -> Bool
checkExpr = undefined


--
-- * Denotational semantics
--

-- ** Store

-- The denotational semantics of Imp uses a store to keep track of the value
-- of each variable. A store is a mapping from variable names to integers.
-- I have provided the implementation of the store for you.

-- | A store is an updateable mapping from variable names to integers.
type Store = [(Var,Int)]

-- | Initialize the store with all of the declared variables.
new :: [Var] -> Store
new = map (\x -> (x,0))

-- | Lookup a variable in the store.
get :: Var -> Store -> Int
get x s = maybe (notFound x) id (lookup x s)

-- | Set the value associated with a variable in the store.
set :: Var -> Int -> Store -> Store
set x _ []        = notFound x
set x i ((y,j):s) = if x == y then (x,i) : s else (y,j) : set x i s

-- | Throw a runtime error if a variable is not found in the store. We do this
--   rather than use a lifted domain since any program that passes our static
--   checking phase will not trigger a runtime error.
notFound :: Var -> a
notFound x = error ("Variable " ++ x ++ " not found in store!")


-- ** Fixpoint

-- You'll also need the fixed point function for the semantics of while loops.
-- I've provided that for you as well.

-- | Compute the least fixed point. (Also defined in Data.Function).
fix :: (a -> a) -> a
fix f = let x = f x in x


-- ** Valuation functions

-- Imp expressions and tests use the state of the store to return integer and
-- boolean values, respectively. Statements update the state of the store.
-- Most of the specific constructs should be self-explanatory.
--
-- You can assume that the dynamic semantics is only defined for Imp programs
-- that pass the static check. That is, a variable reference or assignment is
-- guaranteed to succeed, since we know all used variables were declared.

-- 4. Implement a denotational semantics for Imp. For each syntactic category
--    (expressions, tests, statements, and programs) you should identify the
--    semantic domain, then implement the valuation function.


-- | Semantics of integer expressions.
--   Semantic domain: ??
expr = undefined

-- | Semantics of boolean expressions.
--   Semantic domain: ??
test = undefined

-- | Semantics of statements.
--   Semantic domain: ??
stmt = undefined

-- | Semantics of programs.
--   Semantic domain: ??
prog = undefined
