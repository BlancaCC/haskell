# ¿Cuál es el número más grande? 

Descartes: Ese número no existe
Supongamos que existiera, sea G el número más grande, pero (G+1) existe y es mayor que G, por tanto hemos llegado a una contradicción. 
Haskell: 
```
Prelude> a = 1/0
Prelude> 1000 < a
True
Prelude> filter (a<=) [1..]
--(nunca acaba)
```
