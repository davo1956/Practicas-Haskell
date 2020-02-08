
--funciones de tarea que pueden serlo 
concatena [] [] = []
concatena [] ys = ys
concatena (x:xs)ys = x :(concatena xs ys)

sumaElem lista = [ x | x<-lista ]

reversa lista = reverse[x | x<-lista ]

sumLista [] = 0
sumLista (x:xs) = x + sumLista xs

reve [] = []
reve (x:xs) = reve (xs) ++ [x]



