-- factorial de un numero
fact :: Int -> Int
fact 1 = 1
fact 0 = 1
fact n = n * fact(n-1)

-- multiplicacion de dos numeros
mult :: Int -> Int -> Int
mult a 0 = 0
mult a b = a + mult a (b-1)

-- potencia de un num.
pot :: Int -> Int -> Int
pot a 0 = 1 
pot a b = a * pot a (b-1)

-- suma de dos numeros
suma :: Int -> Int -> Int
suma a 0 = a
suma a b = 1 + (suma a (b-1))

-- cociente de la div
cociente :: Int -> Int -> Int
cociente a b = if a < b then 0 else 1 + cociente (a-b) b

-- modulo de una div
modulo :: Int -> Int -> Int
modulo a b = if a < b then a else modulo (a-b) b

-- con listas
l1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
l2 = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
l3 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

miLength :: [ta] -> Int
miLength [] = 0
miLength (x:xs) = 1 + miLength xs

miMap :: (ta -> tf) -> [ta] -> [tf]
miMap f [] = []
miMap f (x:xs) = (f x):(miMap f xs)

miFilter :: (ta -> Bool) -> [ta] -> [ta]
miFilter f [] = []
miFilter f (x:xs) = if f x then x : (miFilter f xs) else miFilter f xs

miTakeWhile :: (ta -> Bool) -> [ta] -> [ta]
miTakeWhile f [] = []
miTakeWhile f (x:xs) = if f x then (x : (miTakeWhile f xs)) else miTakeWhile f []

miDropWhile :: (ta -> Bool) -> [ta] -> [ta]
miDropWhile f [] = []
miDropWhile f (x:xs) = if f x then ([]++ (miDropWhile f xs)) else xs

miZip :: [ta] -> [tb] -> [(ta, tb)]
miZip [] [] = []
miZip (x:xs) (y:ys) = (x, y) : (miZip xs ys)

miZipWith :: (ta -> tb -> tf) -> [ta] -> [tb] -> [tf]
miZipWith f [] [] = []
miZipWith f (x:xs) (y:ys) = (f x y) : (miZipWith f xs ys)

-- ordenado con funcion de orden > , <
ordenado :: (Int -> Int -> Bool) -> [Int] -> Bool
ordenado f [] = True
ordenado f [_] = True
ordenado f (x:y:xs) = f x y && ordenado f (y:xs)

---- rotar n veces a la izquierda
rotarIzq :: (Num t, Eq t) => t -> [a] -> [a]
rotarIzq 0 xs = xs
rotarIzq n xs = rotarIzq (n-1) (last xs : init xs)

---- rotar n veces a la derecha 
rotarDer :: (Num t, Eq t) => t -> [a] -> [a]
rotarDer 0 xs = xs
rotarDer n xs = rotarDer (n-1) (tail xs ++ [head xs])

-- concat
-- ++
-- !!
