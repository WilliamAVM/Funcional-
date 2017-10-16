--ejemplos

no :: Bool -> Bool
no True = False
no _ = True

ejemplito :: [Int]
ejemplito = let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)] in [a+b | (a,b) <- xs]

--3
reta :: (Int, Int, Int) -> Int
--reta (_,_,5) = 5  --prueba
reta (a, _, _) = a

--4
--quebra :: (Int, Int) -> (Int, Int) -> (Int, Int)
quebra q1@(a,b) q2@(c,d) | u > v = q1
                         | otherwise = q2
						 where
							u = a/b
							v = c/d