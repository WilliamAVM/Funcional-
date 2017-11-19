head' :: [a] -> a
head' [] = error "No puedes utilizar head con una lista vacía!"
head' (x:_) = x

tail' :: [a] -> [a]
tail' [] = []
tail' (x:xs) = xs 

length' :: [a] -> Int
length' [] = 0
length' (x:xs)= 1 + length' xs 

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' n (x:xs) = x:rs where rs = take' (n-1) xs 

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' n (x:xs) = drop' (n-1) xs 

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) = if (f x) then x:(takeWhile' f xs) else takeWhile f []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (x:xs) = if (f x) then dropWhile' f xs else (x:xs)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) = if (f x) then x:filter' f xs else filter' f xs

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = (f x):(map' f xs)

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' (x:xs) (y:ys)= (x,y):(zip' xs ys)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y):(zipWith' f xs ys)

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ (concat' xss)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

union' :: [a] -> [a] -> [a]
union' [] ys = ys
union' (x:xs) ys = x:(union' xs ys)

indice' :: [a] -> Int -> a
indice' [] _ = error "la lista está vacia"
indice' (x:xs) n = if n /= 0 then indice' xs (n-1) else x 

borrar' :: Eq a => a -> [a] -> [a]
borrar' y [] = []
borrar' y (x:xs) = if y==x then (borrar' y xs) else x:(borrar' y xs) 

------------------------------------------------------------------------
rotarDer :: Int -> [a] -> [a]
rotarDer 0 xs= xs
rotarDer n xs= (last xs):rotarDer (n-1) (init xs)    

tamIguales :: [a] -> [b] -> Bool
tamIguales xs ys = if (length' xs) == (length' ys) then True else False

esMatriz :: [[a]] -> Bool 
esMatriz [] = True
esMatriz (xs:xss) = esMatriz' xs xss
esMatriz' _ [] = True
esMatriz' xs (ys:yss) = if tamIguales xs ys then esMatriz' xs yss else False

printHead :: [[a]] -> [a]
printHead (xs:xss) = if esMatriz (xs:xss) then xs else error "esto no es Matriz"


printDiagonal :: [[a]] -> [a]
printDiagonal xss = diagonal 0 xss
diagonal _ [] = []
diagonal n (xs:xss) = ((!!) xs n): diagonal (n+1) xss 

printDiagonal' :: [[a]] -> [a]
printDiagonal' (xs:xss) = diagonal ((length' (xs:xss))-1) (xs:xss)
diagonal' _ [] = []
diagonal' n (xs:xss) = ((!!) xs n) : diagonal (n-1) xss 


sumaVectores ::Num a => [a] -> [a] -> [a]
sumaVectores [] _ = []
sumaVectores (x:xs) (y:ys) = (x+y):sumaVectores xs ys

printMatriz :: [[a]] -> [a]
printMatriz [] = []
printMatriz (xs:xss) = xs ++ printMatriz xss

--sumaMatrices ::Num a => [[a]] -> [[a]] -> [[a]]
--sumaMatrices [] _ = []
--sumaMatrices (xs:xss) (ys:yss) 

