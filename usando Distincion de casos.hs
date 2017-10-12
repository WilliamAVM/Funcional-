--1.- definir 1 funcion que reciba 4 numeros y devuelva el mayor:
--combinacion
funmay2 :: Int->Int->Int
funmay2 a b = if a >b then a else b
funmayde4 :: Int->Int->Int->Int->Int
funmayde4 a b c d = if ( funmay2 a b) > ( funmay2 c d)
  then ( funmay2 a b)
  else ( funmay2 c d)
funmayde4S a b c d  = (funmay2(funmay2 a b) (funmay2 c d))
--distincion de casos
funmayde4DC :: Int->Int->Int->Int->Int
funmayde4DC a b c d |a > b && a > c && a > d =a
  |b > a && b > c && b > d =b
  |c > a && c > b && c > d =c
  |d > a && d > b && d > c =d

--2.- funcion q reciba 1nota y devuelva apribado o reprobado
funAR :: Int->String
funAR a |a >50 && a <101="aprobado"
  |a<=50 && a>=0 ="reprobado"
  |otherwise ="ok..."

--3.- funcion q reciba 1nota y devuelva el mensaje "excelente" esta / 90-100 "bien" si esta entre 70-89, "regular"
--entre 51-69 y mal si esta entre 0-50
funNota :: Int->String

funNota a |a>89 && a<101 ="Excelente"
  |a>69 && a <90 = "Bien"
  |a>50 && a<70 ="regular"
  |a<51 && a>=0 = "Mal"
  |otherwise = "Uff men esas notas"

--4.- funcion q reciba 1parcial 2parcial final y segunda instancia y retorne aprobado o reprobado segun el caso
funNotaF :: Int->Int->Int->Int->String

funNotaF pp sp f si | (pp + sp) >=51 = "aprobado"
  |f > 50 ="aprobado"
  |si >50 = "aprobado"
  |otherwise= "reprobado"
--5.-funcion que reciba 16 numero y devuelva el mayor
funmayde16 :: Int->Int->Int->Int->Int->Int->Int->Int->Int->Int->Int->Int->Int->Int->Int->Int->Int

--a >(funmayde4DC b c d e) && a > (funmayde4DC f g h i) && a> (funmayde4DC j k l m)
funmayde16 a b c d e f g h i j k l m n o p | a >(funmayde4S b c d e) && a > (funmayde4S f g h i) && a> (funmayde4S j k l m) && a> n && a>o && a>p =a
  | b >(funmayde4S a c d e) && b > (funmayde4S f g h i) && b > (funmayde4S j k l m) && b > n && b>o && b>p =b
  |  c >(funmayde4S b a d e) && c > (funmayde4S f g h i) && c> (funmayde4S j k l m) && c> n && c>o && c>p =c

--6.- definir una funcion q reciba un qbrado y devuelva verdad si es mayor q 1 y falso en otro caso
--funqbrado :: (Double->Double)->Bool
funqbrado (a,b) | (a / b) > 1 = True
 | otherwise = False

--7.-funcion q reciba 2 fechas y retorne el mayor
funmayd2fechas :: (Int,Int,Int)->(Int,Int,Int)->(Int,Int,Int)
funmayd2fechas (d1,m1,a1)(d2,m2,a2) | d1 > d2 && m1>m2 && a1 > a2 = (d1,m1,a1)
 | d2 > d1 && m2>m1 && a2 > a1 = (d2,m2,a2)
 |otherwise = (999999999,999999999,999999999)

--8.- funcion q recibe 2 fechas y retorna los años transcurridos
funAtrans ::  (Int,Int,Int)->(Int,Int,Int)->Int
funAtrans (d1,m1,a1)(d2,m2,a2) | a1> a2 = a1 - a2
 | a2 > a1 = a2 - a1

--9.- funcion q recibe 2 fechas y retorna los meses transcurridos
funMtrans :: (Int,Int,Int)->(Int,Int,Int)->Int
funMtrans (d1,m1,a1)(d2,m2,a2) | m1> m2 = m1 - m2
 | m2 > m1 = m2 - m1

 --10.- funcion q recibe 2 fechas y retorna los dias transcurridos
funDtrans :: (Int,Int,Int)->(Int,Int,Int)->Int
funDtrans (d1,m1,a1)(d2,m2,a2) | d1> d2 = d1 - d2
 | d2 > d1 = d2 - d1

--11.-funcion q recibe 2 fechas y retorna los dias,meses,años transcurridos
funAMDtrans :: (Int,Int,Int)->(Int,Int,Int)->(Int,Int,Int)
funAMDtrans (d1,m1,a1) (d2,m2,a2) = (funDtrans (d1,m1,a1) (d2,m2,a2), funMtrans (d1,m1,a1) (d2,m2,a2), funAtrans (d1,m1,a1) (d2,m2,a2))
