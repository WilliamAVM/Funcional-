mayor2 :: Int -> Int -> Int
mayor2 a b = if (a >= b) then a else b

mayor3 :: Int -> Int -> Int -> Int
mayor3 a b c = if (a > b) then ( if(a > c) then a else c ) 
               else (if (b > c) then b else c)

--mayor a b c = mayor2 (mayor2 a b) c
mayor4 :: Int -> Int -> Int -> Int -> Int
mayor4 a b c d = if (a > b) then ( if a > c then (if a > d then a else d) else (if c > d  then c else d))
                 else (if b > c then (if b > d then b else d) else (if c > d then c else d))

aprobacion :: (Int, Int, Int, Int) -> Int -> Int -> Int -> String
aprobacion (a, b, c, d) r p f = if(par > 50 || (( (f + p)`div` 2 ) > 50 || r > 50 )) then "Aprobado" else (if (par == 0 && f == 0) then "Deshabilitado" else "Reprobado")
                                  where par = (a+b+c+d+p) `div` 5 

fm :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
fm f1@(a1,m1,d1) f2@(a2, m2, d2) = if(a1 > a2) then f1 else (if(a1 == a2) then ( if(m1 > m2) then f1 else (if(m1 == m2) then ( if(d1 > d2) then f1 else f2) else f2)) else f2)							