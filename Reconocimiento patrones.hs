--1.-recibe fecha y retorna dia
frn :: Int->String
frn 1 = "lunes"
frn 2 = "Martes"
frn 3 = "Miercoles"
frn 4 = "Jueves"
frn 5 = "Viernes"
frn 6 = "Sabado"
frn 7 = "Domingo"
frn _ = "WTF !!"

--2.-recibe fecha y retorna mes
frm :: Int->String
frm 1 = "Enero"
frm 2 = "Febrero"
frm 3 = "Marzo"
frm 4 = "Abril"
frm 5 = "Mayo"
frm 6 = "Junio"
frm 7 = "Julio"
frm 8 = "Agosto"
frm 9 = "Septiembre sin fap"
frm 10 = "Octubre"
frm 11 = "Noviembre sin ella :'V"
frm 12 = "Navidad"
frm _ = "WTF !!"

--3.-recibe fecha y retorna año
fra :: Int->String
fra 1999 = "mil novecientos noventa y nueve"
fra 2000 = "dosmil"
fra _ = "WTF !!"

--4.-recibe 2 quebrado y retorna mayor
--fqd2 :: (Int->Int)->(Int->Int) ->(Int->Int)
fqd2 q1@(n1,d1) q2@(n2,d2)
 |n1 > n2 && d1 > d2 =q1
 |n2 > n1 && d2 > d1 =q2
 |otherwise = (n1*n2,d1*d2)

 --5.-recibe 1 quebrado y retorna su reducido
--fqr :: (Int->Int)->(Int->Int)
fqr (n,d) = if n`mod`2==0 && d `mod`2==0
            then (n`div`2,d`div`2)
            else if n`mod`3==0 && d `mod`3==0
              then (n`div`2,d`div`2)
              else if n`mod`5==0 && d `mod`5==0
                then (n`div`5,d`div`5)
                else (n,d)
{- |(n`mod`2 == 0 && d `mod` 2== 0   )= (n/2,d/2)
 |(n`mod`3 == 0 && d `mod` 3== 0   )= (n/3,d/3)
 |(n`mod`5 == 0 && d `mod` 5== 0   )= (n/5,d/5)
 |otherwise = (n,d)-}
{-fqr (n,d)
 |divisa n d = 2
 |otherwise = 0
 where

} --a|n `mod` 2 ==0 && d `mod` 2 = (n`div`2,d`div`2)-}

--6.-recibe 1 quebrado y retorna su signo en char
--fqrch ::(Int->Int)->Char
{-fqrch (n,d) | signo n d = '+'
 |signo n d = '-'
 |otherwise ='f'
 where
   signo = if n>0 || d >0 then True else False
-}
fqrch (n,d) | n>0 && d>0 ='+'
 |n<0 && d<0 ='-'
 |otherwise ='!'

--7.- el mayor de 3 fechas
fmd3f q1@(d1,m1,a1) q2@(d2,m2,a2) q3@(d3,m3,a3)
 |d1>d2 && d1>d3 && m1>m2 && m1>m3 && a1>a2 && a1 > a2 = q1
 |d2>d1 && d2>d3 && m2>m1 && m2>m3 && a2>a1 && a2 > a3 = q2
 |d3>d2 && d3>d1 && m3>m2 && m3>m1 && a3>a2 && a3 > a1 = q3

--8.-el mayor de 2 horas
fmd2h ho1@(h1,m1,s1) ho2@(h2,m2,s2)
 |h1> h2 && m1> m2 && s1> s2= ho1
 |otherwise = ho2
--9.-el instante mass sercano de dos instante
--finst ::(Int->Int->Int->Int)->(Int->Int->Int->Int)->(Int->Int->Int->Int)
finst fe1@(f1,h1,m1,s1) fe2@(f2,h2,m2,s2)
 |f1>f2 && h1>h2 && m1>m2 && s1>s2 = fe2
 |f2>f1 && h2>h1 && m2>m1 && s2>s1 = fe1
 |otherwise = (f1,h2,m1,s2)

--10.-reciba # natural y retorne el siguiente
fnts ::Int->Int
fnts a = a+1
--11.- reciba 2 qbrados y retorne su simplificacion
fr2q ((n1,d1),(n2,d2))= ((fqr(n1,d1)),(fqr(n2,d2)))
--fqr ( fqr(n1,d1) fqr(n2,d2))
