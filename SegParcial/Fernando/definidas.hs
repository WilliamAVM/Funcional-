data Lista a = Vacia | Add a (Lista a) deriving (Show)

l1 = (Add 4 (Add 3 (Add 2 (Add 1 Vacia))))
l2 = (Add 8 (Add 7 (Add 6 (Add 5 Vacia))))
l3 = (Add 1 (Add 2 (Add 3 (Add 4 (Add 8 (Add 6 (Add 7 (Add 9 Vacia))))))))

head' :: Lista a -> a 
head Vacia = error "No hay head de lista Vacia "
head' (Add x xs) = x

tail' :: Lista a -> Lista a 
tail' Vacia = Vacia
tail' (Add x xs) = xs 

length' :: Lista a -> Int 
length' Vacia = 0
length' (Add x xs) = 1 + length' xs 

take' :: Int -> Lista a -> Lista a
take' 0 _ = Vacia
take' n (Add x xs) = (Add x (take' (n-1) xs))

drop' :: Int -> Lista a -> Lista a 
drop' 0 xs = xs
drop' n (Add x xs) = drop' (n-1) xs 

takeWhile' :: (a -> Bool) -> Lista a -> Lista a 
takeWhile' f Vacia = Vacia
takeWhile' f (Add x xs) = if (f x) then (Add x (takeWhile' f xs)) else Vacia

dropWhile' :: (a -> Bool) -> Lista a -> Lista a 
dropWhile' f Vacia = Vacia
dropWhile' f (Add x xs) = if (f x) then (dropWhile' f xs) else xs 

filter' :: (a -> Bool) -> Lista a -> Lista a 
filter' f Vacia = Vacia
filter' f (Add x xs) = if (f x) then (Add x (filter' f xs)) else (filter' f xs)

map' :: (a -> b) -> Lista a -> Lista b  
map' f Vacia = Vacia
map' f (Add x xs) = (Add (f x) (map' f xs))

zip' :: Lista a -> Lista b -> Lista (a,b)
zip' Vacia _ = Vacia 
zip' (Add x xs) (Add y ys) = (Add (x,y) (zip' xs ys))

zipWith' :: (a -> b -> c) -> Lista a -> Lista b -> Lista c 
zipWith' f Vacia _ = Vacia
zipWith' f _ Vacia = Vacia
zipWith' f (Add x xs) (Add y ys) = (Add (f x y) (zipWith' f xs ys))


union' :: Lista a -> Lista a -> Lista a 
union' Vacia ys = ys 
union' (Add x xs) ys = (Add x (union' xs ys))

--no puedo el concat

reverse' :: Lista a -> Lista a 
reverse' Vacia = Vacia
reverse' (Add x xs) = (union' (reverse' xs) (Add x Vacia))

indice' :: Lista a -> Int -> a 
indice' Vacia _ = error "no hay wn"
indice' (Add x xs) n = if n == 0 then x else indice' xs (n-1)