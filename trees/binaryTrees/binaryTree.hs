{-
Binary trees in haskell
Funtions:
- Definition
- isBinT (if an element is in a bt
- listToBinTree
- drawBinTree


Example:
*Main> drawBinTree (listToBinTree [1..6])
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
listToBinTree (x:xs) = Node x (listToBinTree (r)) (listToBinTree (l))
  where
    (r,l) = divList xs
    divList :: [a] -> ([a],[a])
    divList [] = ([],[])
    divList [x] = ([x],[])
    divList (x:y:zs) = (x:xs, y:ys)
      where
        (xs,ys) = divList zs


--old implementation of divList:
    {-
partList :: [a] -> [a]
        partList [] = []
        partList (x:[])= x:[]
        partList (x:_:[]) = [x]
        partList (x:_:xs) = x:(partList xs)
-}
-- draw Tree
space :: Int -> [Char]
space rep = take (2*rep) (repeat ' ')

drawBinTree :: Show a => BinTree a -> IO()
drawBinTree t = drawBinTreeAux t 0
  where 
    drawBinTreeAux :: Show a => BinTree a -> Int -> IO ()
    drawBinTreeAux VoidB _ = putStr ""
    drawBinTreeAux (Node n l r) rep = do
      -- "beauty" index 
      putStrLn((space rep) <> "|")
      putStrLn ((space rep) <>"|-"<>(show n))

      drawBinTreeAux l (rep+1)
      drawBinTreeAux r (rep+1)
    -- where  
    space :: Int -> [Char]
    space rep = take (2*rep) (repeat ' ')

