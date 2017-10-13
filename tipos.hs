--f33:: (Int->Int->Int)->Int->Int->Int

funcion n1 n2 n3 b1 n4 b2 = if b1==1 && b2==1 then n1+n2+n3+0 else n4+0

f22 x y z = x y z

f2 x y z = 2 * x + y + z

ej1 x y z | x=y | y=z

incr x = x+1

--c f (x,y) = f x y

ff x y z = x(y z) (y z)

suma incr x y = incr x +y

f3 x y z = x>2 && ((y+1)>z)

mel  x y z | x = y | z=20

f5 x y | x=(*) | otherwise =y



f6 x y z | x=(\a->2*a) | y=z







tf2 f a b c d = if (b && d) then (a+c) else (f a) +1 0 

--4
c f (x, y) = f x y

--5
u f x y = f (x, y)
sum1(a, b) = a + b


f7 x = x


f8 x y= (x 5)+(y True)

ej2 x y z= if y==2 then x + z else 10	

curr f = g
	where g x y = f (x, y) 

uncurr f = g
	where g (x, y) = f x y

curr1 f = g
	where g x y z = f (x, y, z)

fesp e x y z w = if x && (y x) then z else w z
	where w a | a = e
			  | otherwise = z