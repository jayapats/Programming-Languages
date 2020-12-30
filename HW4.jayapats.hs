module HW3 where

import MiniMiniLogo
import Render


--
-- * Semantics of MiniMiniLogo
--

-- NOTE:
--  * MiniMiniLogo.hs defines the abstract syntax of MiniMiniLogo and some
--    functions for generating MiniMiniLogo programs. It contains the type
--    definitions for Mode, Cmd, and Prog.
--  * Render.hs contains code for rendering the output of a MiniMiniLogo
--    program in HTML5. It contains the types definitions for Point and Line.

-- | A type to represent the current state of the pen.
type State = (Mode,Point)

-- | The initial state of the pen.
start :: State
start = (Up,(0,0))

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toHTML ls


-- Semantic domains:
--   * Cmd:  State -> (State, Maybe Line)
--   * Prog: State -> (State, [Line])


-- | Semantic function for Cmd.
--
--   >>> cmd (Pen Down) (Up,(2,3))
--   ((Down,(2,3)),Nothing)
--
--   >>> cmd (Pen Up) (Down,(2,3))
--   ((Up,(2,3)),Nothing)
--
--   >>> cmd (Move 4 5) (Up,(2,3))
--   ((Up,(4,5)),Nothing)
--
--   >>> cmd (Move 4 5) (Down,(2,3))
--   ((Down,(4,5)),Just ((2,3),(4,5)))
--
cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen Up) (_,(x,y)) = ((Up,(x,y)), Nothing)
cmd (Pen Down) (_,(x,y)) = ((Down,(x,y)), Nothing)
cmd (Move x y) (d,(m,n)) = ((d,(x,y)),l)
                          where l = case d of
                                      Down -> (Just((m,n),(x,y)))
                                      Up -> Nothing

-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
prog :: Prog -> State -> (State, [Line])
prog [] = \s -> (s,[])
prog (c:cs) = \s -> case cmd c s of
                          (s',Nothing) -> prog cs s'
                          (s',Just l)  -> let (p,ls) = prog cs s' in (p,l:ls)


--
-- * Extra credit
--

-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.


bigbox :: Int -> Int -> Prog
bigbox x y = [Pen Up, Move x y, Pen Down,
           Move (x+40) y, Move (x+40) (y+20), Move x (y+20), Move x y]

box1 :: Int -> Int -> Prog
box1 x y = [Pen Up, Move x y, Pen Down,
            Move (x+10) y, Move (x+10) (y+10), Move x (y+10), Move x y]


amazing :: Prog
--amazing = drawCircle 16 40
amazing = drawHello 30 10

drawHello :: Int -> Int -> Prog
drawHello x y =   nix x y (x) (y+10) ++ nix (x+10) y (x) (y+10) ++ bigbox x y ++ box1 x y ++ box1 (x+10) (y+10) ++ box1 (x+20) (y+10) ++ box1 (x+20) (y) ++ box1 (x+30) (y)
