{-
 Basis definition a tree 
  -}

data Tree a = Void | Node a [ Tree a] deriving Show


----      Example of declaration ----
t0 :: Tree Integer
t0 = Node 0[t1,t2,t3]
  where
    t1 = Node 1 [Node 10 [], Node 11[]]
    t2 = Node 2 [Node 20[]]
    t3 = Node 3 []

-- empty three (using void constructor
emptyThree :: Tree Integer
emptyThree = Void



----- Basic functions ----

-- return the root

root :: Tree a -> a
root Void = error "Empty tree"
root (Node x _ ) = x


-- tree size ( Number of nodes )

size :: Tree a -> Integer
size Void = 0
size (Node _ xs) = 1 + sum (map size xs)

-- tree depth  (largest level)

depth :: Tree a -> Integer
depth Void = 0
depth (Node _ []) = 1
depth ( Node _ xs) = 1 + maximum (map depth xs)

-- Is a leaf?

isLeaf :: Tree a -> Bool
isLeaf ( Node _ []) = True
isLeaf _ = False

-- Find the maximum element of the three
tMaximum :: Ord  a => Tree a -> a
tMaximum Void = error "Empty tree"
tMaximum (Node x []) = x
tMaximum (Node x xs) = maximum( x:( map tMaximum xs) )


-- Sum all the elments of the tree

tSum :: Num a  => Tree a -> a 
tSum Void = error "Empty tree"
tSum (Node x []) = x
tSum (Node x xs) = x + sum( map tSum xs)

