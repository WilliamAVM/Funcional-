{-
	Ejercicios con listas
-}
l = [2, 4, 6, 7, 2, 4, 8, 7, 2]

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
