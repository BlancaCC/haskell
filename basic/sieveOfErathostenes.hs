{-
Sieve of Erathostenes
Blanca Granada 26/X/19
-}

{-  number that are not divided by the first
e.g.
*Main> take 6 (sieve [2..])
[3,5,7,9,11,13]

*Main> take 6 (sieve( sieve[2..]))
[5,7,11,13,17,19]

-}


sieve :: Integral a => [a] -> [a]
sieve (p:xs) = [ x | x <- xs, mod x p /= 0 ]

erathostenes :: [Integer]
erathostenes = map head  (iterate sieve [2..])
