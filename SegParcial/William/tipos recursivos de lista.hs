a = Vacia --0
b = (Agregar 1 (Vacia))--1
c = (Agregar 2 (Agregar 1 (Vacia)))--2
d = (Agregar 3 (Agregar 2 (Agregar 1 (Vacia))))--3
e = (Agregar 4 (Agregar 3 (Agregar 2 (Agregar 1 Vacia))))
f = (Agregar 5 (Agregar 4 (Agregar 3 (Agregar 2 (Agregar 1 Vacia)))))
g = (Agregar 1 (Agregar 2 (Agregar 3 (Agregar 4 Vacia))))
h = (Agregar 10 (Agregar 20 (Agregar 30 (Vacia))))

data Lista a = Vacia | Agregar a (Lista a)
  deriving (Show)

mlhead :: Lista a -> a
mlhead (Agregar x xs) = x

mltail :: Lista a -> Lista a
mltail Vacia = Vacia
mltail (Agregar x xs) = xs

mllast :: Lista a -> a
--mllast Vacia = Vacia
mllast (Agregar x xs )= if ((mllength xs) == 0 )then x else mllast xs

mllength :: Lista a -> Int
mllength Vacia = 0
mllength (Agregar x xs) = 1+mllength xs

mlinit Vacia = Vacia
mlinit (Agregar x xs) = if ((mllength xs) == 0)then ys else (Agregar x (mlinit xs))
  where ys = Vacia

mltake ::Int -> Lista a -> Lista a
mltake 0 xs = Vacia
mltake n (Agregar x xs)= (Agregar x (mltake (n-1) xs))

mldrop :: Int -> Lista a -> Lista a
mldrop 0 (Agregar x xs) = xs
mldrop n Vacia = Vacia
mldrop n (Agregar x xs) = mldrop (n-1) xs

mltakew f Vacia = Vacia
mltakew f (Agregar x xs)= if (f x )then (Agregar x (mltakew f xs)) else (mltakew f Vacia)

mldropw f Vacia = Vacia
mldropw f (Agregar x xs) = if (f x) then mldropw f xs else (Agregar x (mldropw f xs))

mlfilter f Vacia = Vacia
mlfilter f (Agregar x xs) = if (f x) then (Agregar x (mlfilter f xs)) else( mlfilter f xs)

mlmap f Vacia = Vacia
mlmap f (Agregar x xs ) = (Agregar (f x) (mlmap f xs) )

mlzip Vacia Vacia = Vacia
{--
mzip (Agregar x xs) Vacia = xs
mzip  Vacia (Agregar y ys) =ys--}
mlzip (Agregar x xs) (Agregar y ys) = (Agregar (x,y) (mlzip xs ys))

mlzipw f Vacia  Vacia = Vacia
mlzipw f (Agregar x xs) (Agregar y ys) = (Agregar (f x y) (mlzipw f xs ys))

mlconcat (Agregar xs xss)  = xs ++ ((mlconcat xss ))
--no se como putas hacer una lista de listas con la estructura q se nos enseño
--si alguien lo logra q lo pruebe con este metodo

mlrevers Vacia = []
mlrevers (Agregar x xs) = (mlrevers xs) ++ [x]

mlresta Vacia Vacia = Vacia
mlresta xs Vacia = xs
mlresta Vacia ys = ys
mlresta (Agregar x xs) (Agregar y ys) = if x == y
  then mlresta xs ys
  else (Agregar x (mlresta xs ys))

mlunion xs Vacia = xs
mlunion Vacia ys = ys
mlunion xs (Agregar y ys) = mlunion(Agregar y xs) ys

mlinserta n Vacia = (Agregar n Vacia)
mlinsertar n xs = (Agregar n xs)

mlindice (Agregar x xs) n = if a == 0 then x else mlindice xs (n-1)
  where a = (n-1)

mlsumall Vacia = 0
mlsumall (Agregar x xs) =  x +( mlsumall xs)

mlfoldr f n Vacia = n
mlfoldr f n (Agregar x xs) = mlfoldr f (f n x) xs

mfolr f n []= n
mfolr f n (x:xs) = mfolr f (f n x) xs
