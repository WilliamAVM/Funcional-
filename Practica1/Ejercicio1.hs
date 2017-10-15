cuadrado :: Float -> Float
cuadrado l = l*l

rectangulo :: Float -> Float -> (Float, Float)
rectangulo b h = (2*(b+h), b*h)

mayorA :: Float -> Float -> Bool
mayorA a b = a > b

mult2 :: Int -> Bool
mult2 n = mod n 2 == 0

mult23 :: Int-> Bool
mult23 n = (mod n 2 + mod n 3) == 0

multR2 :: Int -> Bool
multR2 n = ((mult2 n) && ((mod n 3) == 0))

pot3 :: Int -> Int
pot3 n = n^3

pots :: Int -> (Int, Int, Int, Int)
pots n = (n^4, n^8, n^10, n^32)

funOrd :: (Int -> Int -> Bool) -> (Int -> Int -> Bool)
funOrd f x y = f x y