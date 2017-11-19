l1=[1,2,3,4,5,6,7]
l11=[7,5,9,5,1,4,6]
l2=[7,6,5,4,3,2,1]
l3=[[1,2,3],[4,5,6],[7,8,9]]

mipoten:: Int->Int->Int
mipoten a 0 = 1
mipoten 0 b = 0
mipoten a b = a*(mipoten a (b-1))

facto ::Int->Int
facto 0 =1
facto n = n*(facto(n-1))

mult2 :: Int->Int->Int
mult2 a 0 = 0
mult2 0 b = 0
mult2 a b = a+(mult2 a (b-1))

sum2 ::  Int->Int->Int
sum2 a 0 = a
sum2 0 b = b
sum2 a b = sum2 (a+1) (b-1)

mhead [] = 0
mhead (x:xs) = x

mtail::[a]->[a]
mtail []=[]
mtail (x:xs)=xs

mlast::[a]->a
mlast (x:xs) = if (length xs)==0 then x else mlast xs

minit1[]=[]
minit1 (x:xs) = if (length xs) == 0 then ys else x:(minit1 xs)
  where ys =[]

minita :: [a] -> [a]
minita [_] = []
minita (x:xs) = x : minita xs

mlengt [] = 0
mlengt (x:xs) =  1+(mlengt xs)

mtake n [] = []
mtake 0 xs =[]
mtake n (x:xs) = x : mtake (n-1) xs

mdrop n [] = []
mdrop 0 xs = xs
mdrop n (x:xs) = if n==0 then xs else mdrop (n-1) xs

mtakew :: (ta -> Bool) -> [ta] -> [ta]
mtakew f [] = []
mtakew f (x:xs) = if f x then x:(mtakew f xs) else mtakew f []

mdropw f [] = []
mdropw f (x:xs) =if f x then mtakew f xs else xs

mfiltr f [] = []
mfiltr f (x:xs) = if f x then x:mfiltr f xs else mfiltr f xs

mmap f [] = []
mmap f (x:xs) = f x :(mmap f xs)

mzip :: [ta] -> [tb] -> [(ta, tb)]
mzip [] [] = []
mzip (x:xs)(y:ys)= (x , y) : mzip xs ys

mzipw f [] [] = []
mzipw f (x:xs) (y:ys)= (f x y) :( mzipw f xs ys)

concat' [[]] =[]
concat' (xs:xss)=  xs ++ (concat' xss)

reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

resta [] [] =[]
resta (x:xs) (y:ys) = if x == y then resta xs ys else x:resta xs ys

union xs [] = xs
union [] ys = ys
union (x:xs) (ys) = if (length xs) >= 0 then x:(union xs ys) else union xs ys

insertar:: Integer -> [Integer]-> [Integer]
insertar n [] = [n]
insertar n xs = xs++[n]

indice :: [Integer]-> Integer -> Integer
indice (x:xs) n = if a == 0 then x else indice xs (n-1)
  where a= n-1

sumall [] =0
sumall (x:xs)=x+ sumall xs

mfolr f n []= n
mfolr f n (x:xs) = mfolr f (f n x) xs 
