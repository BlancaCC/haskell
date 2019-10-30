{-
     Typeclass basic definitions

-}

---- Definition of the typeclass ---- 
class BasicEq a where     -- for all the types a, as long as "a" is an instance of BasisEq 
  isEqual :: a -> a -> Bool
  me :: a -> a
  

--- Instance BasicEq ---
instance BasicEq Bool where
  --- isEqual DEfinicition 
  isEqual True True = True
  isEqual False False = True
  isEqual _ _  = False
  --- mirrow instance 
  mirrow x = x



data Mood = Happy | Chill | Sad

-- If we want me function work we need to instance show
instance Show Mood where
  show Happy = "HAPPY"
  show Chill = "-zzzzz"
  show Sad = "You may think you are SAD, That's not true :)"

  
  
instance BasicEq Mood where
  -- isEqual Definition 
  isEqual Happy Happy = True
  isEqual Chill Chill = True
  isEqual Sad Sad = True
  isEqual _ _ = False

-- mirrow
  mirrow x = Happy

{-
instance Read Mood where
  -- readPrec is the main function for parsing input 
  readsPrec _ value =
    tryParse [("happy", Happy), ("chill",Chill), ("sad", Sad)] -- math that tryParse will try
    where tryParse [] = []
          tryParse ( (attempt, result), xs) 








  
-}
