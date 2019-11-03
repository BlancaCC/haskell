{-
Binary trees in haskell
Funtions:
- Definition
- isBinT (if an element is in a bt
- listToBinTree
- drawBinTree


Example:
*Main> drawBinTree (listToBinTree [1..6]) 0
|
|-1
  |
  |-2
    |
    |-4
    |
    |-6
  |
  |-3
    |
    |-5


Date: Granda 3/X/19
-}


--- Definition ---
data BinTree t = VoidB | Node t (BinTree t) (BinTree t) deriving Show


-- declaration of a Bin tree 
bt :: BinTree Integer
bt = Node 0 (Node 1 VoidB VoidB) VoidB


---- other function -----

-- determinate if a is in a BinTree

isInBinT:: Eq a => a -> BinTree a -> Bool
isInBinT a VoidB = False
isInBinT a (Node n l r)
  | a == n = True
  | otherwise = (isInBinT a l) || (isInBinT a r)



-- create a BinTree from a list
listToBinTree :: [a] -> BinTree a
listToBinTree [] = VoidB
listToBinTree (x:xs) = Node x (listToBinTree (partList xs)) (listToBinTree (partList (drop 1 xs)))
  where partList :: [a] -> [a]
        partList [] = []
        partList (x:[])= x:[]
        partList (x:_:[]) = [x]
        partList (x:_:xs) = x:(partList xs)


  
-- draw Tree
space :: Int -> [Char]
space rep = take (2*rep) (repeat ' ')
  
drawBinTree VoidB _ = putStr ""
drawBinTree (Node n l r) rep =do
  putStrLn((space rep) <> "|")
  putStrLn ((space rep) <>"|-"<>(show n))

  drawBinTree l (rep+1)
  drawBinTree r (rep+1)
 
