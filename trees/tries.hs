{-
TRIES

Is a data structure to store keys


https://es.wikipedia.org/wiki/Trie
-}
data Trie a e = Void | Transition [a] [Trie]| Key [a] e [Trie]

insertKey :: Trie -> a -> b -> Trie 
insertKey Void = Key a b 


  
