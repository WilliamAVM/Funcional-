l1= [[1,2],[3,4]]
l2= [[5,6],[7,8]]
l3=[[1,2,3],[4,5,6],[7,8,9]]
l4=[[9,8,7],[6,5,4],[3,2,1]]
--1)Defina una funcion que reciba una lista de listas y devuelva
--el primer elemento de la primera lista
primero :: [[a]]->a
primero xs = head(head xs)

--2)Defina una funcion que reciba una lista de listas y devuelva
--el tercer elemento de la segunda lista
terdelseg:: [[a]]->a
terdelseg xs = (!!) ((!!) xs 1) 2

--3)Defina una funcion que reciba una lista de listas de listas
-- y devuelva el primer elemento de la penultima lista de la
--antepenultima lista

pri::[[[a]]]->a
pri zs =primero(drop (length (head (drop (length (zs)-3) zs))-2) (head (drop (length (zs)-3) zs)) )

--suma2mat:: [[a]]->[[a]]->[[a]]
suma2mat [] [] = []
suma2mat (x:xs) (y:ys) = [(head x)+(head y)]:(suma2mat (x:xs) (y:ys))

--transpuesta de una matriz
col xss i= map (!! i) xss
fila yss i= (!!) yss i
trans xss = map (col xss)[0..length xss-1]


multiplica :: [[Integer]] -> [[Integer]] -> [[Integer]]
multiplica m1 m2 = mult m1 (trans m2)
mult [] _ = []
mult (xs:xss) yss = [hacerFila xs yss] ++ (mult xss yss)
hacerFila xs [] = []
hacerFila xs (ys:yss) = (creaElem xs ys):(hacerFila xs yss)
creaElem [] [] = 0
creaElem (x:xs) (y:ys) = (x*y) + (creaElem xs ys)
trans xss = trp xss 0
trp xss i | i == length xss = []
		  | otherwise = [formFil xss i]++(trp xss (i+1))
formFil [] i = []
formFil (xs:xss) i = (xs !! i):(formFil xss i)


m1 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
m2 = [[2, 3, 4], [7, 8, 9], [1, 2, 3]]
