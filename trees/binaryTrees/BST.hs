{-
Binary search tree is an especial binary tree that satisfy:

- can be the Void Node
- all elements fromt its left subtree are less or equal
- all elemnts from its right subtree are greater
-}


-- Structur declaration 
data BinT a = VoidB | Node a (BinT a) (BinT a)

-- Declaration of some tree
notBST :: BinT Integer
notBST = Node 0 (Node 1 VoidB VoidB) VoidB

notBST2 :: BinT Integer
notBST2 =  Node 3 (Node 2 (Node 1 VoidB VoidB) VoidB) (Node 4  VoidB (Node 4 VoidB VoidB))

aBST :: BinT Integer
aBST =  Node 3 (Node 2 (Node 1 VoidB VoidB) VoidB) (Node 4  VoidB (Node 5 VoidB VoidB))

-- determine if a binary tree is a bst
isBST :: Ord a => BinT a -> Bool
isBST VoidB = True
isBST (Node a l r) = cmpDown ( <= a ) l && cmpDown (> a) r && isBST r && isBST l

cmpDown :: (a -> Bool) -> BinT a -> Bool
cmpDown p VoidB = True
cmpDown p (Node e l r) = p e && cmpDown p r && cmpDown p l

-- insert an element

--insertBST :: a -> binT a -> binT a
--insertBST a VoidB = Node a VoidB VoidB

--insertBST :: Ord a => a -> BST a -> BST a
--insertBST e VoidBST = Node e VoidBST VoidBST


