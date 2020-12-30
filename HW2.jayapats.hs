module HW2 where

-- | Binary trees with nodes labeled by values of an arbitrary type.
data Tree a
   = Node a (Tree a) (Tree a)
   | End
  deriving (Eq,Show)

-- | One step in a path, indicating whether to follow the left subtree (L)
--   or the right subtree (R).
data Step = L | R
  deriving (Eq,Show)

-- | A path is a sequence of steps. Each node in a binary tree can be
--   identified by a path, indicating how to move down the tree starting
--   from the root.
type Path = [Step]

-- | Create a leaf node.
leaf :: a -> Tree a
leaf x = Node x End End

-- | An example tree.
ex :: Tree Int
ex = Node 4 (Node 3 (leaf 2) End)
            (Node 7 (Node 5 End (leaf 6))
                    (leaf 8))


-- | Encode a list as a tree with only right branches.
--
--   >>> encodeList []
--   End
--
--   >>> encodeList [1,2,3,4]
--   Node 1 End (Node 2 End (Node 3 End (Node 4 End End)))
--
--   >>> encodeList ":-D"
--   Node ':' End (Node '-' End (Node 'D' End End))
--
encodeList :: [a] -> Tree a
encodeList [] = End
encodeList(x:xs) = Node x (End) (encodeList xs)

-- | Map a function over a tree. Applies the given function to every label
--   in the tree, preserving the tree's structure.
--
--   >>> mapTree odd End
--   End
--
--   >>> mapTree even (Node 5 (leaf 2) End)
--   Node False (Node True End End) End
--
--   >>> (mapTree not . mapTree even) (Node 5 End (leaf 2))
--   Node True End (Node False End End)
--
--   >>> mapTree (+10) ex
--   Node 14 (Node 13 (Node 12 End End) End) (Node 17 (Node 15 End (Node 16 End End)) (Node 18 End End))
--
--   >>> ex == (mapTree (subtract 27) . mapTree (+27)) ex
--   True
--
mapTree :: (a->b) -> Tree a -> Tree b
mapTree f End = End
mapTree f (Node a l r) = Node (f a) (mapTree f l) (mapTree f r)


-- | Get the value at the node specified by a path. Returns 'Nothing' if
--   the given path is invalid.
--
--   >>> valueAt [] ex
--   Just 4
--
--   >>> valueAt [L,L] ex
--   Just 2
--
--   >>> valueAt [L,R] ex
--   Nothing
--
--   >>> valueAt [R,L,R] ex
--   Just 6
--
--   >>> valueAt [L,L,L] ex
--   Nothing
--

--ex = Node 4 (Node 3 (leaf 2) End)
--            (Node 7 (Node 5 End (leaf 6))
--                    (leaf 8))

-- Step 1 - Base case- returning Nothing for invalid
-- Step 2 - If the Path is empty returning the root node
-- Step 3 - If the Path consists of L, then we recursively call the valueAt for left subtree
-- Step 4 - If the Path consists of R, then we recursively call the valueAt for left subtree

valueAt :: Path -> Tree a -> Maybe a
valueAt _ End = Nothing
valueAt [] (Node a l r)= Just a
valueAt (L:xs) (Node a l r) = valueAt xs l
valueAt (R:xs) (Node a l r) = valueAt xs r

-- | Find a path to a node that contains the given value.
--
--   >>> pathTo 3 (leaf 5)
--   Nothing
--
--   >>> pathTo 5 ex
--   Just [R,L]
--
--   >>> pathTo 6 ex
--   Just [R,L,R]
--
--   >>> pathTo 4 ex
--   Just []
--
--   >>> pathTo 10 ex
--   Nothing
--

-- Step 1 - At the first step we are cheking if the start node is matching. If it matches then we are returning an empty lists.
-- Step 2 - In above case, if not matches then we are checking the left sub tree. If the value is present, then we are appending the xs list with L
-- Step 3 - Else we are checking the right sub tree and, if present we are appending R to the XS

pathTo :: Eq a => a -> Tree a -> Maybe Path
pathTo a End = Nothing
pathTo a (Node x l r)
      | a == x = Just []
      | otherwise = case pathTo a l of
                      Just xs  -> Just (L:xs)
                      Nothing -> case pathTo a r of
                                   Just xs  -> Just (R:xs)
                                   Nothing -> Nothing
