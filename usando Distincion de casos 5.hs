--12.- Deinir una funcion q reciba 4 # y retorne el mayor
--a)combinacion
--funmay4 ::Int->Int->Int->Int->Int
funmay4 a b c d =if a>b && a>c && a>d
  then a
  else if b>a && b>c && b>d
    then b
    else if c>a && c>b && c>d
      then c
      else d
--b)distincion de casos
{-
funmay4DC ::Int->Int->Int->Int->Int
funmay4DC a b c d |a >b && a>c && a>d = a
 |b >b && a>c &&a>d =
-}
--13.- Deinir una funcion q reciba 1 # y retorne Aprobado o reprobado
funAproRepro :: Int-> String
funAproRepro n | n>50 && n<101= "Aprobado"
 |n>0 && n<51 = "Reprobado"

 --14.- Deinir una funcion q reciba 4 notas y retorne Aprobado o mal etc
funnotas :: Int->Int->Int->Int->String
funnotas w x y z | alav >= 90 && alav <= 100 = "Excelente"
 |alav>=70 && alav <=89 ="Bien"
 |alav>=51 && alav <=69 = "Regular"
 |alav>=0 && alav<=50 ="Mal"
 where
  alav=((w+x+y+z)`div`4)

--15.-Deinir una funcion q reciba notas de evaluacion continua, final, y retorne Aprobado o mal etc
funEvConFin :: Int->Int->String
funEvConFin prs fns | fns<=25 || prs<=25 = "Deshabilitado"
 |fns > 50 || prs > 50= "aprobado"
 |fns < 50 || prs < 50= "Reprobado"

--16.-Deinir una funcion q reciba notas de evaluacion continua, y retorne Aprobado o mal etc
funNtFunc::Int->Int-> String
funNtFunc a b |prom >25 ="habilitado"
 | prom <= 25="deshabilitado"
 where
   prom = (a+b)`div`2

{-17.- funcion q reciba 16# retorne el mayor
funMay16 :: Int->Int->Int->Int->Int->Int->Int->Int->Int->Int->Int->Int->Int->Int->Int->Int->Int
funMay16 a b c d e f g h i j k l m n o p |may164>=0 = 0
 where
  may164 q w r t y u o s z x v ñ mn nr od pa= (funmay4((funmay4 q w r t )(funmay4 y u o s )(funmay4 z x v ñ )(funmay4 mn nr od pa)))
-}

--18.-funcion q reciba un quebrado y retorne true si es mayor q 1 y falso en otro caso
funsqrt'::(Int,Int)-> String
funsqrt' (a,b) |promsqrt >1 ="True"
 | otherwise ="False"
 where
   promsqrt = a`div`b
--19.-funcion q reciba 2 fechas y retorne la mayor entre las dos
fechaMayd2 :: (Int,Int,Int)->(Int,Int,Int)->(Int,Int,Int)
fechaMayd2 (d1,m1,a1) (d2,m2,a2) | mayfe>(d2,m2,a2) =(d1,m1,a1)
  |otherwise = (d2,m2,a2)
  where
    mayfe = if (d1 > d2 && m1 > m2 && a1>a2) then (d1,m1,a1)
      else(d2,m2,a2)

--20-. funcion q reciba 2 fechas y retorne los anos transcurridos
anosTras :: (Int,Int,Int)->(Int,Int,Int)->Int
anosTras (d1,m1,a1) (d2,m2,a2) | a1>a2 =a1-a2
  |otherwise = a2-a1

--21.-funcion q reciba 2 fechas y retorne los meses transcurridos
mesTras :: (Int,Int,Int)->(Int,Int,Int)->Int
mesTras (d1,m1,a1) (d2,m2,a2) | m1>m2 =m1-m2
  |otherwise = m2-m1

--22.-funcion q reciba 2 fechas y retorne los dias transcurridos
disTras :: (Int,Int,Int)->(Int,Int,Int)->Int
disTras (d1,m1,a1) (d2,m2,a2) | d1>d2 =d1-d2
  |otherwise = d2-d1

--22.-funcion q reciba 2 fechas y retorne los dias,mese anos transcurridos
dmaTras :: (Int,Int,Int)->(Int,Int,Int)->(Int,Int,Int)
dmaTras (d1,m1,a1) (d2,m2,a2) | d1>d2 && m1>m2 && a1>a2 =(d1-d2,m1-m2,a1-a2)
  |otherwise = (d2-d1,m2-m1,a2-a1)
