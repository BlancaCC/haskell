pideLetra = do
  putStr "Introduce una letra y te la duplicaré: "
  c<- getChar
  if c == 'a'
    then putStr "\nAAAAAAAAAAAAAA has dicho mi letra fav\n"
    else do
    putChar c
    putChar '\n'

          


-- Crear secuencia de caracteres 
todoList = [putChar 'a',
            do
              putChar 'a'
              putChar '\n',
            do
              c <- getChar
              putStr (take 10 (repeat c))]
