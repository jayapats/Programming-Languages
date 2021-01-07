module HW3 where
import Prelude hiding (length,sum,product,map,foldr,group)

-- | Convert a regular list into a run-length list.
--
--   >>> compress [1,1,1,2,3,3,3,1,2,2,2,2]
--   [(3,1),(1,2),(3,3),(1,1),(4,2)]
--
--   >>> compress "Mississippi"
--   [(1,'M'),(1,'i'),(2,'s'),(1,'i'),(2,'s'),(1,'i'),(2,'p'),(1,'i')]
--
--compress :: Eq a => [a] -> [(Int,a)]
--compress =

--foldr :: (a -> b -> b) -> b -> [a] -> b
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f b []     = b
foldr f b (x:xs) = f x (foldr f b xs)

addOne x = x + 1

compress :: Eq a => [a] -> [(Int,a)]
compress []       = []

compress (x:xs) = compress 1 x xs where
    compress n x [] = [(n, x)]
    compress n x (y:ys)
        | x == y    = compress (n + 1) x ys
        | otherwise = (n, x) : compress 1 y ys
--------------------------working
-- compress [] = []
-- compress (x:xs) = reverseList [x] x xs
--   where
--   reverseList b c [] = [reverse b]
--   reverseList b c (y:ys)
--    | y == c    = reverseList (y:b) c ys
--    | otherwise = reverse b : reverseList [y] y ys



--compress =  let y = length x head x : foldr xs
--compress [(x:xs)] = [(length x, head x) | x <- group xs]
-- | Convert a run-length list back into a regular list.
--
--   >>> decompress [(5,'a'),(3,'b'),(4,'c'),(1,'a'),(2,'b')]
--   "aaaaabbbccccabb"
--
decompress :: [(Int,a)] -> [a]
-- decompress ((x, a): xs) = show a when if x !=0
--                            decrement the value of x and iterate the loop
-- Tried to implement with foldr - iterating the loop and printing the second value of the tupple the number of times of the first value.
