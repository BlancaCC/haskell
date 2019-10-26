{- FILTRO DE PASO BAJO 
Este programa consiste en crear un filtro, dado un número máximo y una lista de enteros, decuelve otra lista con los menores
Granada  30 /III / 18
-}

-- Mi solución::

f :: Int -> [Int] -> [Int]
f _ [] = []

f max (x:xs)
  | x < max = [x] ++ f max xs
  |otherwise = f max xs

--OTRAS SOLUCIONES DE HACKER DE HACKERRANK

-- Solución de Boerworz, utiliza método filter,aunque se especificaba que preferentemente no 
f1 n = filter (<n)

-- Sillogismo, por compresión ME GUSTA
f2 n l = [x | x<- l, x<n]

-- la forma que subí a Hackerrrank, no se porqué no me deja la estructura del principio :_ 
f3 :: Int -> [Int] -> [Int]
f3 _ [] = []
f3 max (x : xs) = 
    if x < max 
        then [x]++ f max xs
        else f max xs
  
 
