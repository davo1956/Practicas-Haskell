data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving Show

sumTree :: Num a => Tree a -> a
sumTree (Leaf a) = a 
sumTree (Node a b c) = a + (sumTree b) + (sumTree c)


sumList :: Num p => [p] -> p
sumList [] = 0
sumList (x:xs) = x + sumList xs



reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa (xs) ++ [x]



eliminaElem :: Eq a => [a] -> a -> [a]
eliminaElem [] a = []
eliminaElem (x:xs) a = if (x == a)
                       then eliminaElem xs a
                       else [x] ++ eliminaElem xs a



hasElem :: Eq t => [t] -> t -> Bool
hasElem [] t = False
hasElem (x:xs) t = if (x == t)
                   then True
                   else hasElem xs t
{-hasElem (Leaf a) b= if a == b
                      then true
                      then false
 hasElm (Node a b c) m= if m == a
 then true
 else (hasElm b m) || (hasElm c m)
-}

hasElemTree :: Eq t => Tree t -> t -> Bool
hasElemTree (Leaf a) t = if (a == t)
                         then True
                         else False
hasElemTree (Node a b c) t = if (a == t) 
                               then True
                             else if ((hasElemTree b t) == True)
                               then True
                             else if ((hasElemTree c t) == True)
                               then True
                               else False



preOrden :: Tree a -> [a]
preOrden (Leaf a) = [a]
preOrden (Node a b c) = [a] ++ preOrden b ++ preOrden c

concatena :: [a] -> [a] -> [a]
concatena [] [] = []
concatena [] ys = ys
concatena (x:xs)ys = x :(concatena xs ys)


