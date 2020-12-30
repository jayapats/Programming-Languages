-- This is the 381 study session :-)
module ExamOne where

type State = Int

type Val = State -> State

type I = Int
data S = Inc I | Reset
type P = [S]

valS :: S -> Val
valS (Inc a) = \s -> s + a
valS Reset = \s -> 0

-- [Inc 5, Reset, Inc 2]
-- (Inc 5):Reset:(Inc 2):[]
--
-- p -> 5
--
-- s = Inc 5
-- p -> 0
--
-- s = Reset
-- p -> 2
--
-- s = Inc 2
-- p -> 0
--
-- 0
valP :: [S] -> Int -> Int
valP [] x    = x
valP (s:p) x =
    let
        f = valS s
        newX = f x
    in
        valP p newX

-- valP' :: [S] -> Int
-- valP' [] = 0
-- valP' (s:p) = valS s (valP' p)
-- 
-- valP :: [S] -> Int
-- valP p = valP' (reverse p)

-- reverse
-- 2 * (1 + 3)
--
--

data Cmd = Gas | Brake | Turn
type Prog = [Cmd]

type Pos   = Int
type Speed = Int
data Dir   = Forward | Backward

type StateRobot = (Pos, Maybe (Speed, Dir))
type ValRobot = StateRobot -> StateRobot


-- Gas: Move in the current direction an amount equal to the current speed, then increase the speed by one. For example, if the robot is at position 5 while moving forward at a speed of 2, after executing a Gas command the robot would be at position 7 moving at a speed of 3.

flipD :: Dir -> Dir
flipD Forward = Backward
flipD Backward = Forward

semCmd :: Cmd -> ValRobot
semCmd Gas = \s ->
    case s of 
        (p, Nothing) -> (p, Nothing)
        (p, Just (speed, Forward)) -> (p + speed, Just (speed+1, Forward))
        (p, Just (speed, Backward)) -> (p - speed, Just (speed+1, Backward))
semCmd Brake = \s ->
    case s of
        (p, Nothing) -> (p, Nothing)
        (p, Just (0, d)) -> (p, Just (0, d))
        (p, Just (speed, Forward)) -> (p + speed, Just (speed-1, Forward))
        (p, Just (speed, Backward)) -> (p - speed, Just (speed-1, Backward))
semCmd Turn = \s ->
    case s of
        (p, Nothing) -> (p, Nothing)
        (p, Just (0, d)) -> (p, Just (0, flipD d))
        (p, Just _) -> (p, Nothing)

semProg :: Prog -> StateRobot -> StateRobot
semProg [] x = x
semProg (c:p) x = semProg p (semCmd c x)
