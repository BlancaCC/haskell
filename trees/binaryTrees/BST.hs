{-
Binary search tree is an especial binary tree that satisfy:

- can be the Void Node
- all elements fromt its left subtree are less or equal
- all elemnts from its right subtree are greater
-}


-- Structur declaration 
data BinT a = VoidB | Node a (BinT a) (BinT a) deriving Show

-- Declaration of some tree
notBST :: BinT Integer
notBST = Node 0 (Node 1 VoidB VoidB) VoidB

notBST2 :: BinT Integer
notBST2 =  Node 3 (Node 2 (Node 1 VoidB VoidB) VoidB) (Node 4  VoidB (Node 4 VoidB VoidB))

aBST :: BinT Integer
aBST =  Node 3 (Node 2 (Node 1 VoidB VoidB) VoidB) (Node 4  VoidB (Node 5 VoidB VoidB))

voidt :: BinT Integer
voidt = VoidB

-- determine if a binary tree is a bst
isBST :: Ord a => BinT a -> Bool
isBST VoidB = True
isBST (Node a l r) = cmpDown ( <= a ) l && cmpDown (> a) r && isBST r && isBST l

cmpDown :: (a -> Bool) -> BinT a -> Bool
cmpDown p VoidB = True
cmpDown p (Node e l r) = p e && cmpDown p r && cmpDown p l

-- insert an element
insertBST ::Ord a=> a -> BinT a -> BinT a
insertBST a VoidB = Node a VoidB VoidB
insertBST a (Node n l r) 
  | a <= n = Node n (insertBST a l) r
  | otherwise = Node n l (insertBST a r)

  
{- Example:
*Main> foldr insertBST bt [1..5]
Node 5 (Node 4 (Node 3 (Node 2 (Node 1 VoidB VoidB) VoidB) VoidB) VoidB) VoidB
*Main> foldl (flip insertBST) voidt [1..3]
Node 1 VoidB (Node 2 VoidB (Node 3 VoidB VoidB))

-}

-- flip the argument of insertBST
--  insertBST2 t e = flip insertBST t e
insertBST2 :: Ord a => BinT a -> a -> BinT a
insertBST2 t e = insertBST e t
{- Example 
*Main> foldl insertBST2 voidt [1..3]
Node 1 VoidB (Node 2 VoidB (Node 3 VoidB VoidB))
-}




