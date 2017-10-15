cmay2 :: Int -> Int -> Int 
cmay2 a b | a > b = a
          | otherwise = b

cmay4 :: Int -> Int -> Int -> Int -> Int
cmay4 a b c d = cmay2 d (cmay2 c (cmay2 a b))

may4 :: Int -> Int -> Int -> Int -> Int
may4 a b c d | a >= b && a >= c && a >= d = a
             | b >= a && b >= c && b >= d = b
			 | c >= a && c >= b && c >= d = c
             | otherwise = d

nota :: Int -> String
nota n | n > 50 = "Aprobado"
       | otherwise = "Reprobado"

nota1 :: Int -> String
nota1 n | entre 90 100 = "Excelente"
        | entre 70 89 = "Bien"
		| entre 51 70 = "Regular"
        | entre 0 50 = "Reprobado"		
         where entre i s = n >= i && n <= s

fechas :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
fechas (a1, m1, d1) (a2, m2, d2) = (at, mt, dt) 
                                  where 
                                  at | a1 > a2 = a1 - a2
                                           | otherwise = a2 - a1
                                  --where  NON!
                                  mt | m1 > m2 = m1 - m2
                                     | otherwise = m2 - m1
                                  --where   NON!
                                  dt | d1 > d2 = d1 - d2
                                     | otherwise = d2 - d1

fechaALT :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)									 
fechaALT f1@(a1, m1, d1) f2@(a2, m2, d2) | (a1 > a2) || (a1 == a2 && m1 > m2) || ((a1 == a2) && (m1 == m2) && d1 > d2) = f1
                                         | otherwise = f2