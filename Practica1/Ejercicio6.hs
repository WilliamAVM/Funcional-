--13
fde6 :: Int -> Int -> Int->Int -> Int -> Int-> Int
fde6 a b c d e f = mini where mini = (min f (min e (min d (min c (min a b)))))

fdelet6 :: Int -> Int -> Int->Int -> Int -> Int-> Int
fdelet6 a b c d e f = let mini = (min f (min e (min d (min c (min a b))))) in mini

--16a
f177 :: Int
f177 = z where z = 177

--16b
-- fraro :: Bool -> Bool -> Int
-- fraro p q = let
			   -- res = 0 
			-- in
			   -- | p         = 77 - res
			   -- | q         = 17 - res 
			   -- | otherwise = res

fraro :: Bool -> Bool -> Int
fraro p q | p = 2 - res
          | q = 17 * res
		  | otherwise = res
		  where
		       res = 11
			   
frarol :: Bool -> Bool -> Int
frarol p q = let res | p = (-) 2
                     | q = (*) 17
					 | otherwise = (+) 0
			 in res 11