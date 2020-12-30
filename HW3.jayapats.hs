module HW3 where

import Data.List

--Q1
type Numm = Int
type Var = String
type Macro = String

type Prog = [Cmd]

data Mode = Up | Down
          deriving (Eq,Show)

data Expr = VRef Var
          | Rnum Numm
          | Add Expr Expr
        deriving (Eq,Show)

data Cmd = Pen Mode
         | Move Expr Expr
         | Define Macro [Var] Prog
         | Call Macro [Expr]
       deriving (Eq,Show)


--2a Concrete Syntax
 --define line (x1,y1,x2,y2) {
  --pen up; move (x1,y1);
  --pen down; move (x2,y2);
--  }

--2b
line :: Cmd
line = Define "line" ["x1","y1","x2","y2"][Pen Up, Move (VRef "x1") (VRef "y1"), Pen Down, Move (VRef "x2") (VRef "y2")]

-- Q3 Concrete Syntax
--  define nix (x,y,w,h) {
--    call line (x, y, x+w, y+h); //Calls the previously defined line to draw from (x,y) to (x+w, y+h)
--    call line (x, y+h, x+w, y); //Calls the previously defined line to draw from (x,y+h) to (x+w, y)
--  }

--Q3b
nix :: Cmd
nix = Define "nix" ["x","y","w","h"] [
  Call "line" [VRef "x", VRef "y",
               Add (VRef "x") (VRef "w"),
               Add (VRef "y") (VRef "h")],
  Call "line" [VRef "x", Add (VRef "y") (VRef "h"),
               Add (VRef "x") (VRef "w"), VRef "y"]]

--steps :: Int -> Prog
--steps 0 = [Move VRef "0" VRef "0"]

move :: Int -> Int -> Cmd
move x y = Move (Rnum x) (Rnum y)

steps :: Int -> Prog
steps 0 = [Pen Up, move 0 0, Pen Down]
--steps 0 = [move 0 0]
--steps n = steps (n-1) ++ [Pen Up, move (n-1) n, move n n, Pen Down]
steps n = steps (n-1) ++ [Pen Up, move (n-1) n, Pen Down, move n n]

--GHCI
--steps 4
--[Pen Up,Move (Rnum 0) (Rnum 0),Pen Down,Pen Up,Move (Rnum 0) (Rnum 1),Pen Down,Move (Rnum 1) (Rnum 1),Pen Up,Move (Rnum 1) (Rnum 2),Pen Down,Move (Rnum 2) (Rnum 2),Pen Up,Move (Rnum 2) (Rnum 3),Pen Down,Move (Rnum 3) (Rnum 3),Pen Up,Move (Rnum 3) (Rnum 4),Pen Down,Move (Rnum 4) (Rnum 4)]

macros :: Prog -> [Macro]
macros = concatMap macrosL
  where
    macrosL (Define m [v] p) = m : macros p
    macrosL _              = []



--Pretty Printing

-- Printing Program
pretty :: Prog -> String
--pretty p = intercalate ";" (map prettyCmd p) ++ ";" -- adds ;
pretty p = intercalate ";\n" (map prettyCmd p)


-- Printing Commands
prettyCmd :: Cmd -> String
prettyCmd (Pen Up) = "pen up"
prettyCmd (Pen Down) = "pen down"
prettyCmd (Move x y) = "move (" ++ prettyExp x ++ ", " ++ prettyExp y ++ ")"
prettyCmd (Define m v p) = "define " ++ m ++ " (" ++ intercalate ", " v ++ ") {\n" ++ pretty p ++ "\n}"
prettyCmd (Call m e) = "call " ++ m ++ " (" ++ intercalate ", " (map prettyExp e) ++ ")"

-- Printing Expressions
prettyExp :: Expr -> String
prettyExp (VRef v) = v
prettyExp (Rnum n) = show n
prettyExp (Add l r) = prettyExp l ++ "+" ++ prettyExp r

--GHCI
--putStrLn(pretty [Define "line" ["x1","y1","x2","y2"][Pen Up, Move (VRef "x1") (VRef "y1"), Pen Down, Move (VRef "x2") (VRef "y2")]])
--define line (x1, y1, x2, y2) {
--pen up;
--move (x1, y1);
--pen down;
--move (x2, y2);
--}


optE :: Expr -> Expr
optE (Add (Rnum m) (Rnum n)) = Rnum (m + n)
--optE (Add (VRef m) (VRef n)) = Add (VRef m) (VRef n)
optE (Add l r) = case (optE l,optE r) of
--                    (l , _) -> optE l
--                    (_ , r) -> optE r
                    (l , r) -> optE (Add l r)

--optE (Add (Rnum 2) (Rnum 3))
--Rnum 5

--Getting an output when numbers are given, incase of variable I am trying to retain the same.


optProg :: Prog -> Prog
optProg = map optCmd
            where
              optCmd (Move x y) = Move (optE x) (optE y)
              optCmd (Define m v p) = Define m v (optProg p)
              optCmd (Call m e) = Call m (map optE e)

-- GHCI
--optProg [Define "line" ["x1","y1","x2","y2"][Pen Up, Move (VRef "x1") (VRef "y1"), Pen Down, Move (VRef "x2") (VRef "y2")]]
--[Define "line" ["x1","y1","x2","y2"]
