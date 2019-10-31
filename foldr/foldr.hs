{-
Foldr function and derivates as scanr
31/X/19

Why is foldr useful?
There is a recurrent patter that appears frecuently
-}

--- example of patter that appears frecuently ---
mySum :: Num a => [a] -> a 
mySum [] = 0
mySum (x:xs) =(+) x  (mySum xs)

myConcat [] = []
myConcat (x:xs) = x <> (myConcat xs)

-- with fold
mySum2 x = foldr (+) 0 x
myConcat2 x = foldl (<>) "" x

{-
Examples:
*Main> mySum [1..10]
55
*Main> mySum2 [1..10]
55
*Main> myConcat ["h", "o", "l", "a"]
"hola"
*Main> myConcat2 ["h", "o", "l", "a"]
"hola"

-}

--- Definition of foldr ---

{-
*Main> myFoldr  (<>) "hola " ["soy ", "una ", "pringada "]
"soy una pringada hola "
-}
-- how to read this: "Take a function ( f(a,b)-> b ) and element type b and a list of 'a's and return a b type elements
myFoldr ::( a -> b -> b) -> b -> [a] -> b 
myFoldr f z [] = z
myFoldr f z (x:xs) = f x (myFoldr f z xs)


{-
*Main> myFoldl  (<>) "hola " ["soy ", "una ", "pringada "]
"hola soy una pringada "
-}
myFoldl :: ( b -> a -> b) -> b -> [a] -> b
myFoldl f x [] = x
myFoldl f z (x:xs) = myFoldl f (f z x) xs


{-
*Main> myScanr  (<>) "hola " ["soy ", "una ", "pringada "]
["soy una pringada hola ","una pringada hola ","pringada hola ","hola "]

-}
myScanr :: (a -> b -> b) -> b -> [a] -> [b]
myScanr f x [] = [x]
myScanr f z (x:xs) = [myFoldr f z (x:xs)] <> (myScanr f z xs)
{-
*Main> myScanl  (<>) "hola " ["soy ", "una ", "pringada "]
["hola ","hola pringada ","hola una pringada ","hola soy una pringada "]
-}
myScanl :: ( b -> a -> b) -> b -> [a] -> [b]
myScanl f x [] = [x]
myScanl f z (x:xs) = (myScanl f z xs) <> [(myFoldl f z(x:xs))]



-- List of x \in N-{0} first factorial number
listFactorial :: (Num b, Enum b) => b -> [b]
listFactorial x = myScanl (*) 1 [1..x]
