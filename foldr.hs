
l1 = [2, 3, 4, 5, 6, 7, 8] 
l2 = [9,8,7,6, 5,4, 3, 2]

miFilter :: (tx-> Bool) -> [tx] -> [tx]
miFilter f xs = foldr g a xs
	where
		a = []
		g x rs = if f x then x:rs else rs

miMap :: (tx -> tf) -> [tx] -> [tf]
miMap f xs = foldr g a xs
	where
		a = []
		g x ls = (f x) : ls

miReverse :: [ta] -> [ta]
miReverse xs = foldr g a xs
	where
		a = []
		g x ls = ls ++ [x]

unir :: [tx] -> [tx] -> [tx]
unir xs ys = foldr g a xs
	where
		a = ys
		g x ls = x:ls

miTakeWhile :: (tx -> Bool) -> [tx] -> [tx]
miTakeWhile f xs = foldr g a xs
	where
		a = []
		g x ls = if f x then x:ls else []

mayor :: [Int] -> Int
mayor xs = foldr g a xs
	where
		a = head xs
		g x s = if x > s then x else s

ordena :: [Int] -> [Int]
ordena xs = foldr g a xs
	where
		a = []
		g x ls = (takeWhile (<=x) ls) ++ (x:dropWhile(<=x)ls)

lis1 = [1, 2, 3, 4, 5, 6, 7, 8]
lis2 = lis1

compara xs ys = if l1 /= l2 then False else and(foldr f a [0..l1-1])
	where 
		a = []
		f i ls = ((xs !! i) == (ys !! i)):ls
		l1 = length xs
		l2 = length ys