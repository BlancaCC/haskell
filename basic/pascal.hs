{-
print pascal triangle
e.g:
*Main> pascal 5
1 
1 1 
1 2 1 
1 3 3 1 
1 4 6 4 1 
1 5 10 10 5 1 

Blanca, Granada 26/X/19
-}


-- PASCAL VERSION 2 --

pascal2  = [1] : map f pascal2
  where f xs = zipWith (+) (0: xs) ( xs ++ [0])
  
pascalShow s = putStrLn (unlines (map show (take s pascal2)))



-- PASCAL VERSIÓN 1 -- 
-- read number from standar input 
getInt :: IO Int
getInt = do
  line <- getLine
  let a= ( read line :: Int)
  return (a+1)



-- factorial funtion
f :: Int -> Int
f 0 = 1
f x = product [1..x]

-- combination with repetition 
cr :: Int -> Int -> Int
cr n r = f n `div`(f r * f(n-r) )



-- return a list with pascal with r-th row
fila :: Int -> [Int]
fila  r = [ cr r i | i<- [0..r] ]

pascalAux::Int -> Int -> IO ()
pascalAux k i
  | i < k  = do
    putStrLn (concat (map (<>" ")( map show (fila i))))
    pascalAux k (i+1)
  |otherwise = putStrLn (concat (map (<>" ")( map show (fila k))))

pascal :: Int -> IO ()
pascal k = pascalAux k 0

