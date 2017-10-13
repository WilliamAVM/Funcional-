{-
	Ejercicios FOLDR
-}

l1 = [1, 2, 3, 4, 5, 6]

sumatoria :: [Integer] -> Integer
sumatoria xs = foldr g 0 xs
	where g r s = r + s

productoria :: [Integer] -> Integer
productoria xs = foldr (*) 1 xs

miConcat :: [[a]] -> [a]
miConcat xss = foldr (++) [] xss

miLength xs = foldr f a xs
	where
		a = 0
		f x r = 1 + r