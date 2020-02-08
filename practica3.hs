data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving Show

concatena3 :: [a] -> [a] -> [a]
concatena3 [] [] = []
concatena3 (x:y:z:xs) [] = z : concatena3 xs []
concatena3 [] (x:y:z:xs) = z : concatena3 [] xs
concatena3 (x:y:xs) [] = []
concatena3 (x:xs) [] = []
concatena3 [] (x:y:xs) = []
concatena3 [] (x:xs) = []
concatena3 (x:y:z:xs) (a:b:c:ys) = (z : concatena3 xs []) ++ (c : concatena3 ys [])

mayorQue :: Ord a => [a] -> a -> [a]
mayorQue [] n = []
mayorQue (x:xs) n = if (x > n)
                    then x : mayorQue xs n
                    else mayorQue xs n

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x,y] = if (x < y)
                 then True
                 else False
isSorted (x:xs) = if (isSorted xs == True)
                  then True
                  else False

postOrden :: Tree a -> [a]
postOrden (Leaf a) = [a]
postOrden (Node a b c) = postOrden b ++ postOrden c ++ [a]

forAllExistsDivide :: Integral a => [a] -> Bool
forAllExistsDivide [] = False
forAllExistsDivide [x] = False
forAllExistsDivide [x,y] = if (divide y x == True || divide x y == True)
                           then True
                           else False
forAllExistsDivide (x:y:xs) = if (divide y x == True)
                              then forAllExistsDivide (y:xs)
                              else if (divide x y == True)
                                then forAllExistsDivide (x:xs)
                                else forAllExistsDivide ((y:xs) ++ [x])

existsForAllDivide :: Integral a => [a] -> Bool
existsForAllDivide [] = False
existsForAllDivide [x] = False
existsForAllDivide [x,y] = if (divide y x == True || divide x y == True)
                           then True
                           else False
existsForAllDivide (x:y:xs) = if (divide y x == True)
                              then existsForAllDivide (x:xs)
                              else if (divide x y == True)
                                then existsForAllDivide (y:xs)
                                else existsForAllDivide ((y:xs) ++ [x])

existsUniqueForAllDivide :: Integral a => [a] -> Bool
existsUniqueForAllDivide [] = False
existsUniqueForAllDivide [x] = False
existsUniqueForAllDivide [x,y] = if (divide y x == True || divide x y == True)
                                 then if (x == y)
                                   then False
                                   else True
                                 else False
existsUniqueForAllDivide (x:y:xs) = if (divide y x == True)
                                    then if (y == x)
                                      then existsUniqueForAllDivide ((y:xs) ++ [x])
                                      else existsUniqueForAllDivide (x:xs)
                                    else if (divide x y == True)
                                      then if (x == y)
                                        then existsUniqueForAllDivide ((x:xs) ++ [y])
                                        else existsUniqueForAllDivide (y:xs)
                                      else existsUniqueForAllDivide ((y:xs) ++ [x])
--Función auxiliar para saber si x divide a y.
divide :: Integral a => a -> a -> Bool
divide x y = if (mod y x == 0)
            then True
            else False

isTreeSorted :: Integral a => Tree a -> Bool
isTreeSorted (Leaf a) = True
isTreeSorted (Node a b c) = if (isSorted (preOrden (Node a b c)) == True)
                            then True
                            else False
--Función auxiliar para obtener la lista del recorrido preOrden de un árbol.
preOrden :: Tree a -> [a]
preOrden (Leaf a) = [a]
preOrden (Node a b c) = [a] ++ preOrden b ++ preOrden c

createPreOrden :: [a] -> Tree a
createPreOrden [a] = (Leaf a)
createPreOrden [a,b,c] = (Node a (createPreOrden [b]) (createPreOrden [c]))
createPreOrden (a:b:xs) = (Node a (createPreOrden [b]) (createPreOrden xs))

binarySearch :: Ord a => Tree a -> a -> Bool
binarySearch (Leaf a) n = if (a == n)
                          then True
                          else False
binarySearch (Node a b c) n = if (a == n)
                              then True
                              else if (a > n)
                                then binarySearch b n
                                else if (a < n)
                                  then binarySearch c n
                                  else False

