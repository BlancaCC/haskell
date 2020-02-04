-- Operador then 

opThen1 = putChar 'a' >> putChar 'b' >> putChar 'c' >> putChar '\n'

opThen2 =
  putChar 'A' >>
  putChar 'B' >>
  putChar 'C' >>
  putChar '\n'


opDo1 = do
  putChar 'a'
  putChar 'b'
  putChar 'c'
  putChar '\n'


opDo2 = do {   putChar 'A';  putChar 'B';  putChar 'C'; putChar '\n'}


-- Falta tradución con el operador de monoides >>= bind 
