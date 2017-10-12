--Definir una funcion q reciba el lado de un cuadrado y devuekva su area
laxa :: Int -> Int
laxa l = l *l

--funcion q recibe base y altura de un rectangulo y retorne su area y perimetro

arepi' a b = (a * b, b*2 + a*2)

--funcion quue reciba 2 n# si el 1 es mayor q 2 true
may' n1 n2 = n1>n2

--funcion q reciba 1# y retorn true si es multiplo de 2

multi2' n1 = if n1 `mod` 2==0
  then True
  else False

  --funcion q reciba 1# y retorn true si es multiplo de 2 y 3
ejercicioseis' n = if n `mod`2 == 0 && n `mod`3 == 0
  then True
  else False

ala' n = if(n `mod` 3 == 0 && (multi2' n))
    then True
    else False

--7.-definir una  funcion q reciba 1# y q retorn elevado a la potencia de 3
ejerciosiete' n = n*n*n

potencia4' n = (laxa (laxa n ))
--8.-definir funcion q reciba 1# y q retorn elevado a la potencia de 4, 8, 10, 32.

potencia8' n = potencia4' (laxa n)

potencia10' n = (potencia8' n )* (laxa n)

potencia32' n = (potencia10' n)* (potencia10' n) * (potencia10' n) * laxa n

--9.-definir funcion q reciba 2# y 1 funcion de orden y q retornverdad si los numeros obedecen a funion de orden falso en caso contrario
funord \f n m = (f (n m )) 
