{-
AVL tree (named after inventors Adelson-Velsky and Landis) is a self-balancing binary search tree.

Balancing means: if we have a bin tree: t0 T1 T2, (t0 the root ant T1,T2 its sons)
and we have defined deep function => |deep T1 - deep T2 | <= 1 for all subtrees in the structure

Granada 17/XI/19
Blanca
-}


-- Structure definition

data AVL a = Void | Node a (AVL a) (AVL a) deriving Show

lr :: AVL Integer
lr = Node 2 (Node 1 Void Void) (Node 4 (Node 3 Void Void) (Node 5 Void Void))

rr :: AVL Integer
rr = Node 4 (Node 2 (Node 1 Void Void) (Node 3 Void Void)) (Node 5 Void Void)

-- ROTATIONS

{--- left rotation ---
Transform this node: 
       gf
    /     \
 lf        rf
 |        / \
(avl)    ls   rs


to: 
       rf
    /     \
   gf     rs
 /    \
lf    ls   
 |        
(avl)    
-}
leftRotation :: AVL a -> AVL a
leftRotation (Node gf lf (Node rf ls rs)) = (Node rf (Node gf lf ls) rs)
leftRotation _ = error "Not valid AVL structure. Right son should be a Node, not Void"

{-- Right rotation --
This
       gf
    /     \
   lf     rf
 /    \
ls    rs   
 |        
(avl)

to:
       lf
    /     \
 ls        gf
 |        / \
(avl)    rs   rf
-}

rightRotation :: AVL a -> AVL a
rightRotation (Node gf (Node lf ls rs) rf) = (Node lf ls (Node gf rs rf))
rightRotation _ = error "Not valid AVL structure. Left son should be a Node, not Void"

-- Algebra notes: RightRotation <> LeftRotation = Identity


-- Double rotations 

rightLeftRotation :: AVL a -> AVL a
rightLeftRotation (Node x  xs z) = leftRotation (Node x xs (rightRotation z))
rightLeftRotation _ = error "Not valid AVL to make rightLeftRotation"


leftRightRotation :: AVL a -> AVL a
leftRightRotation (Node x y xs) = rightRotation (Node x (leftRotation y) xs)
leftRightRotation _ = error "Not valid AVL to make leftRightRotation"


{- Examples:

-}
