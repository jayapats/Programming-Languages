module StackLang where


import Prelude hiding (Num)


--
-- * Syntax of StackLang
--

-- Grammar for StackLang:
-- 
--    num ::= (any integer)
--   bool ::= `true`  |  `false`
--   prog ::= cmd*
--    cmd ::= num                         push a number on the stack
--         |  bool                        push a boolean on the stack
--         |  `add`                       add the top two numbers the stack
--         |  `mul`                       multiply the top two numbers the stack
--         |  `eq`                        check whether the top two elements are equal
--         |  `if` prog `else` prog `end` if the value on the top is true
--                                        then run the first program, else run the second

-- 1. Encode the above grammar as a set of Haskell data types
--

type Prog = [Cmd]

data Cmd
   = PushN Int
   | PushB Bool
   | Add
   | Mul
   | Equ
   | IfElse Prog Prog
  deriving (Eq,Show)

-- 2. Write the following StackLang program as a Haskell value:
--
--   3 4 add 5 eq
--
ex1 :: Prog
ex1 = [PushN 3, PushN 4, Add, PushN 5, Equ]


-- 3. Write a StackLang program that:
--     * checks whether 3 and 4 are equal
--     * if so, returns the result of adding 5 and 6
--     * if not, returns the value false
--    First write it in concrete syntax, then in abstract syntax
--    as a Haskell value.
--
--    3 4 eq if 5 6 add else false end
--
ex2 :: Prog
ex2 = [PushN 3, PushN 4, Equ, IfElse [PushN 5, PushN 6, Add] [PushB False]]


-- 4. Write a Haskell function that takes two arguments x and y
--    and generates a StackLang program that adds both x and y to
--    the number on the top of the stack.
addXY :: Int -> Int -> Prog
addXY x y = [PushN x, PushN y, Add, Add]
-- addXY x y = [PushN x, Add, PushN y, Add]


-- 5. Write a Haskell function that takes a list of integers and
--    generates a StackLang program that computes the sum of all the integers.
sumNs :: [Int] -> Prog
sumNs []     = [PushN 0]
sumNs (x:xs) = sumNs xs ++ [PushN x, Add]



--
-- * Operational Semantics of StackLang (now!)
--

-- 6. Identify/define the machine state for a StackLang program.

-- data Value = I Int | B Bool
--
-- data Either a b = Left a | Right b

type Value = Either Int Bool
type Stack = [Value]

type State = (Prog, Stack)


-- 7. Define a one-step reduction relation for a StackLang program,
--    and implement it as a function.
step :: State -> Maybe State
-- step ([],s)           = Just ([],s)
step (PushN i : p, s) = Just (p, Left i : s)
step (PushB b : p, s) = Just (p, Right b : s)
step (Add : p, Left i  : Left j  : s)  = Just (p, Left (i+j) : s)
step (Mul : p, Left i  : Left j  : s)  = Just (p, Left (i*j) : s)
step (Equ : p, Left i  : Left j  : s)  = Just (p, Right (i == j) : s)
step (Equ : p, Right b : Right c : s)  = Just (p, Right (b == c) : s)
step (IfElse t e : p, Right True : s)  = Just (t ++ p, s)
step (IfElse t e : p, Right False : s) = Just (e ++ p, s)
step _ = Nothing


-- 8. Implement the reflexive, transitive closure of the one-step
--    reduction to evaluate a StackLang program.
steps :: State -> Maybe State
steps ([], s) = Just ([], s)
steps state   = case step state of
                  Just state' -> steps state'
                  Nothing     -> Nothing

-- | Run a program on an initially empty stack.
--
--   >>> runOS ex2
--   Just [Right False]
--
--   >>> runOS (sumNs [1..10])
--   Just [Left 55]
--
--   >>> runOS [PushN 3, Add, PushN 4]
--   Nothing
--
runOS :: Prog -> Maybe Stack
runOS p = case steps (p,[]) of
            Just (_, s) -> Just s
            Nothing -> Nothing
-- runOS p = fmap snd (steps (p,[]))


--
-- * Denotational Semantics of StackLang (even later)
--

-- 9. Identify/define a semantics domain for Cmd and for Prog.

-- Domain: Stack -> Maybe Stack


-- 10. Define the semantics of a StackLang command (ignore If at first).
cmd :: Cmd -> Stack -> Maybe Stack
cmd (PushN n)    s                       = Just (Left n : s)
cmd (PushB b)    s                       = Just (Right b : s)
cmd Add          (Left i : Left j : s)   = Just (Left (i+j) : s)
cmd Equ          (Left i : Left j : s)   = Just (Right (i == j) : s)
cmd Equ          (Right b : Right c : s) = Just (Right (b == c) : s)
cmd (IfElse t e) (Right True  : s)       = prog t s
cmd (IfElse t e) (Right False : s)       = prog e s
cmd _ _ = Nothing

-- 11. Define the semantics of a StackLang program.
prog :: Prog -> Stack -> Maybe Stack
prog []     s = Just s
prog (c:cs) s = case cmd c s of
                  Just s' -> prog cs s'
                  Nothing -> Nothing


-- | Run a program on an initially empty stack.
--
--   >>> runDen ex2
--   Just [Right False]
--
--   >>> runDen (sumNs [1..10])
--   Just [Left 55]
--
--   >>> runDen [PushN 3, Add, PushN 4]
--   Nothing
--
runDen :: Prog -> Maybe Stack
runDen p = prog p []
