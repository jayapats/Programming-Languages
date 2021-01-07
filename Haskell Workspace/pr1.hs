-- sum of arrays

suml :: [Int] -> Int
suml [] = 0
suml (x:xs) = x + suml xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort (filter (<= x) xs)
         ++ x : qsort (filter (> x) xs)

rev :: [a] -> [a]
rev [] = []
rev(x:xs) = rev xs ++ [x]

qq :: Ord a => [a] -> [a]
qq [] = []
qq (x:xs) = qsort (filter (<= x) xs)
         ++ x : qsort (filter (> x) xs)

double :: Int -> Int
double x = x + x

isZero :: Int -> Bool
isZero x = x==0

data Result
     = OK Int
     | Error
     deriving (Eq,Show)

sdiv :: Int -> Int -> Result
sdiv x 0 = Error
--sdiv x y = OK (x `div` y)
sdiv x y = OK (x `div` y)

data List
   = Nil
   | Cons Int List
  deriving (Eq,Show)

l1 :: List
l1 = Cons 1 (Cons 2 (Cons 3 Nil))

listSum :: List -> Int
listSum Nil        = 0
listSum (Cons h t) = h + listSum t
