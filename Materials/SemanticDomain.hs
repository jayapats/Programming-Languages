-- What is k'?

k' = let x = 9
         y = x
     in let x = 5
            z = y
        in (x,y)

-- Circle names, underline references:

triple :: Int -> Int
triple y = double y + y

double :: Int -> Int
double x = x + x

perimeter :: Int -> Int -> Int
perimeter x y = double x + double y



-- Language with names:
type Name = String
data Val = Vb Bool
         | Vi Int
         deriving (Eq, Show)

type Error = String
ex = Add (Get "x") (Lit 2) -- Env -> Either Error Val?

-- [("x", Vi 5)],   x + x + x + y
-- type Env = Map Name Val
-- type Env = [(Name, Val)]
-- type Env = Name -> Value

data Expr = Lit Int
          | Get Name -- Get name
          | Add Expr Expr -- Add two values
          | Same Expr Expr -- ==

-- Domain of Expr = Env -> Maybe Val

data Stmt = ProgN [Stmt]
          | Set Name Expr
          | If Expr Stmt Stmt
          | Done Expr
-- Domain = Env -> Maybe (Either Val Env)
--
--

type Env = [(Name, Val)]

-- lookup :: String -> [(String, a)] -> Maybe a
-- In python: dict[name] 

semE :: Expr -> Env -> Maybe Val
semE (Lit i) _ = Just (Vi i)
semE (Get n) env = lookup n env
semE (Add l r) env =
    case (semE l env, semE r env) of
        (Just (Vi lv), Just (Vi rv)) -> Just (Vi (lv+rv))
        _ -> Nothing
semE (Same l r) env =
    case (semE l env, semE r env) of
        (Just lv, Just rv) -> Just (Vb (lv == rv))
        _ -> Nothing


semS :: Stmt -> Env -> Either Val Env
semS (ProgN []) env = Right env
semS (ProgN (s:ss)) env =
    case semS s env of
        Left v ->  
        Right e -> .. 
semS Set Name Expr
semS If Expr Stmt Stmt
semS Done Expr

type Prog = [Expr]
















-- data Cmd
--     = PushInt Int
--     | Pop
--     | AddCmd
-- 
-- type Stack = [Int]
-- 
-- type Domain = Stack -> Maybe Stack
-- 
-- sem :: Cmd -> Domain
-- sem (PushInt i) = \s -> Just (i:s)
-- sem Pop = \s ->
--     case s of
--         [] -> Nothing
--         (_:xs) -> Just xs
-- sem AddCmd = \s ->
--     case s of
--         [] -> Nothing
--         [x] -> Nothing
--         (x:y:xs) -> Just ((x+y):xs)
