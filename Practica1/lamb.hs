--ejemplo
--genera listas dentro de una lista y filtra las que son mayor a 15

chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
	| even n =  n:chain (n `div` 2)  
	| odd n  =  n:chain (n*3 + 1)  

numLongChains :: Int  
numLongChains = length (filter isLong (map chain [1..100]))  
    where isLong xs = length xs > 15 
--LAAAMMMBBBBB-
numLongChainsL :: Int  
numLongChainsL = length (filter (\xs -> length xs > 15) (map chain [1..100]))

--1.5
mult2 :: Int -> Bool
mult2 = (\a -> (a `mod` 2) == 0)

--1.9

forden2 :: (Int->Int->Bool) -> Int -> Int -> Bool
forden2 = (\f a b -> f a b) 

--4.1

sigv :: Char -> Char
sigv = (\a -> case a of 'a' -> 'e'
                        'e' -> 'i' 
                        'i' -> 'o' 
                        'o' -> 'u' 
                        'u' -> 'a' )
						
--4.11
sumatoria :: Int-> Int-> Int-> String
sumatoria = (\a b c ->let r = ((a+b+c)`div`10) in case (r) of 1 -> "Mayor"
                                                              0 -> "Menor"
                                                              otherwise -> "Vacio")
