--1.- Definir funciones que devuelvan el mayor de 2 numeos
ifmayorentr2 :: Int->Int->Int
ifmayorentr2 a b = if a>b then a else b

--2.-Definir funciones que devuelvan el mayor de 3 numeos
ifmayorentr3 :: Int->Int->Int->Int
ifmayorentr3 a b c = if c > (ifmayorentr2 a b )
  then c
  else ifmayorentr2 a b

--3.-Definir funciones que devuelvan el mayor de 4 numeos
ifmayorentr4 :: Int-> Int-> Int->Int->Int
ifmayorentr4 a b c d =if d > ifmayorentr3 a b c
  then d
  else ifmayorentr3 a b c

--10.-Definir funciones que reciba 4 notas devuelvan aprobado reprobado o Dashabilitado segun el caso
promediode4 :: Int-> Int -> Int -> Int -> Int
promediode4 a b c d = (a+b+c+d) `div` 4

funcion4notas :: Int-> Int -> Int -> Int -> String
funcion4notas a b c d = if  promediode4 a b c d > 50 then "aprobado" else
                        if promediode4 a b c d <25
                          then "Deshabilitado"
                          else "reprobado"

--11.-Definir funciones que reciba 2 fechas devuelvan al mayor
--metodo 1
fechaMay1 (d1, m1, a1) (d2, m2, a2) = if a2 > a1 then (d2, m2, a2) else
									 if a2 < a1 then (d1, m1, a1) else
									 if m1 > m2 then (d1, m1, a1) else
									 if m2 > m1 then (d2, m2, a2) else
									 if d1 > d2 then (d1, m1, a1) else
									 	(d2, m2, a2)

fechaMay2 (d1,m1,a1) (d2,m2,a2) = if (d1+m1+a1) > (d2+m2+a2)
  then (d1,m1,a1)
  else (d2,m2,a2)
