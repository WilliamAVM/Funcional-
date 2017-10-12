--13.- definir una funcion donde q reciba 6 numeros y devulva el menor
defLocMin6 :: Int->Int->Int->Int->Int->Int->Int
defLocMin6 a b c d e f |(a >= 0 || a<=0 )=mini6
 |otherwise=0110101010
  where
    mini6 = if a<b && a<c && a<d && a<e && a<f
      then a
      else if b<a && b<c && b<d && b<e && b<f
        then b
        else if c<b && c<a && c<d && c<e && c<f
          then c
          else if d<b && d<a && d<c && d<e && d<f
            then d
            else if e<b && e<a && e<c && e<d && e<f
              then e
              else f

--14.-definir una funcion q reciba 3# y devulva el mensaje sumatoria mayor if menor q 20 sumatoria
--menor if es menor 10 y otherwise vasio
--sumatorias' :: Int -> Int -> Int -> String
sumatorias' a b c | (prome >10 && prome<21) = "Sumatoria mayor"
 |(prome <=10 && prome>0)="Sumatoria menor"
 |otherwise = "Vacio"
  where
   prome = (a + b +c )`div`3

--15.-funcion q reciba 3 notas y retorne excelente si el promedio esta entre 90-100, bien 70-89, bien 51-69
--y mal entre 0-50
notas :: Int -> Int -> Int -> String
notas a b c |prom 90 100 = "Excelente"
  |prom 70 89 = "Bien"
  |prom 51 69 ="Regular"
  |prom 0 51 = "Mal"
  |otherwise = "WTF !!!"
  where
    pr= (a + b +c )`div`3
    prom li ls= pr>=li && pr<=ls
