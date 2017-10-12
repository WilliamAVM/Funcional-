{-
	Ejercicios con listas
-}
l1 = [2, 4, 6, 7, 2, 4, 8, 7, 2]
l2 = [2, 4, 6, 8, 10, 12, 14]
l3 = [4, 4, 4, 4, 4, 4, 4, 4]
l4 = [[1, 2], [3, 4, 5], [6, 7, 8, 9], [10, 11]]

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