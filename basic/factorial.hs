-- Example of factorial funtion implementation
-- 27 / X /10 


factorial :: (Num a, Enum a) => [a]
factorial = 1 : zipWith (*) [1..] factorial



factorial2 :: [Integer]
factorial2 = fact' 1 (1:[1..])
  where fact'  p (x:xs) = q : fact' q xs
         where q = p*x

          
factorial3 = map (head) (iterate (fact'') (1:[1..]))
  where fact'' (p:x:xs) = (p*x):xs
