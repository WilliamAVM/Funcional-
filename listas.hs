{-
	Ejercicios con listas
-}

l1 = [2, 4, 6, 7, 2, 4, 8, 7, 2]
l2 = [2, 4, 6, 8, 10, 12, 14]
l3 = [4, 4, 4, 4, 4, 4, 4, 4]
l4 = [[1, 2], [3, 4, 5], [6, 7, 8, 9], [10, 11]]
l5 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

l6 = [1, 2, 3, 4, 5, 6, 7, 8]
l7 = [1, 2, 3, 4, 5, 6, 7, 8]

-- 1. El seg elemnto
segElem :: [a] -> a
segElem xs = head (tail xs)

-- 2. Antepenultimo elemento
antepen :: [a] -> a
antepen xs = last (init (init xs))

-- 3. Elemento del medio
medio :: [a] -> a
medio xs = (!!) xs ((div) (length xs) 2)

-- 4. La primera mitad de la lista
primMitad :: [a] -> [a]
primMitad xs = take ((div) (length xs) 2) xs

-- 5. Devuelva si el elem. del principio, final y del medio son iguales
iniMedFin :: Eq a => [a] -> Bool
iniMedFin xs = if (ini == med && ini == fin)then True else False
	where
		ini = head xs
		med = (!!) xs ((div) (length xs) 2)
		fin = last xs

-- Otra practica -- FOTOGRAFIA

-- 1. Devolver Verdad si todos los elementos son pares
todosPares :: Integral a => [a] -> Bool
todosPares xs = and (map even xs)

-- 2. Todos los elementos iguales
todosIguales :: Eq a => [a] -> Bool
todosIguales xs = and (map f xs)
	where f x = (head xs) == x

-- 3. Recibe una lista de listas y devuelve una lista con las longitudes
listaLongitud :: [[a]] -> [Int]
listaLongitud xss = map f xss
	where f xs = length xs 

-- 4. Reciba una lista de listas y devuelve True si todas las listas son del mismo tamanio
todasLongIguales :: [[a]] -> Bool
todasLongIguales xss = and (map f xss)
	where f xs = (length (head xss)) == (length xs)

-- 5. Recibe dos listas y devuelve True si son iguales, falso en otro caso
listasIguales :: Eq a => [a] -> [a] -> Bool
listasIguales xs ys = if l1 /= l2 then False else and(zipWith f xs ys)
	where
		l1 = length xs
		l2 = length ys
		f x y = x == y

-- 6. Recibe una lista y devuelve True si esta ordenada ascendentemente
ordenadaAsc :: Ord a => [a] -> Bool
ordenadaAsc xs = and (zipWith (<) xs (tail xs))

-- 7. Recibe una matriz y un valor n y devuelve la columna de la matriz
columnaMatriz :: [[a]] -> Int -> [a]
columnaMatriz xss n = map (!! n) xss

-- 8. Recibe una matriz y devuelva True si la est cuadrada
matrizCuadrada :: [[a]] -> Bool
matrizCuadrada xss = (pe == length xss) && longIguales
	where
		pe = length (head xss)
		longIguales = and(map f xss)
		f x = length x == pe

-- 9. Recibe una matriz y devuelve la diagonal principal
diagonalPrincipal :: [[a]] -> [a]
diagonalPrincipal xss = zipWith (!!) xss ys
	where ys = [0..length(xss)-1]

-- 10. Recibe una matriz y devuelve la diagonal secundaria
diagonalSecundaria :: [[a]] -> [a]
diagonalSecundaria xss = zipWith (!!) xss ys
	where ys = reverse [0..length(xss)-1]

-- 11. Transpuesta de una matriz

transpuesta xss = map f xss
	where 
		f xss = zipWith (!!) xss ys
		ys = [0..(length (head xss))-1]
		
