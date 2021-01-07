module HW3 where


-- | Convert a regular list into a run-length list.
--
--   >>> compress [1,1,1,2,3,3,3,1,2,2,2,2]
--   [(3,1),(1,2),(3,3),(1,1),(4,2)]
-- 
--   >>> compress "Mississippi"
--   [(1,'M'),(1,'i'),(2,'s'),(1,'i'),(2,'s'),(1,'i'),(2,'p'),(1,'i')]
--
compress :: Eq a => [a] -> [(Int,a)]
compress = map entry . chunk
  where entry ys = (length ys, head ys)

-- | Group a list into chunks of identical elements.
--   (This is the 'group' function in the Data.List module.)
--
--   >>> chunk "Missippi"
--   ["M","i","ss","i","pp","i"]
--
chunk :: Eq a => [a] -> [[a]]
chunk []     = []
chunk (x:xs) = let (match, rest) = getAll x xs
               in match : chunk rest

-- | Get all elements that match the given element from the beginning of
--   list. Returns the list of matched elements and the list of remaining
--   elements.
--
--   >>> getAll 2 [2,2,3,4,2,2]
--   ([2,2,2],[3,4,2,2])
--
getAll :: Eq a => a -> [a] -> ([a],[a])
getAll x []     = ([x],[])
getAll x (y:ys) = if x == y then let (match, rest) = getAll x ys
                                 in (y:match, rest)
                  else ([x],y:ys)


-- | Convert a run-length list back into a regular list.
--
--   >>> decompress [(5,'a'),(3,'b'),(4,'c'),(1,'a'),(2,'b')]
--   "aaaaabbbccccabb"
--  
decompress :: [(Int,a)] -> [a]
decompress []         = []
decompress ((n,x):ps) = replicate n x ++ decompress ps

-- decompress = concatMap (uncurry replicate)
