
l1 = [2, 3, 4, 5, 6, 7, 8] 
l2 = [9,8,7,6, 5,4, 3, 2]
miFilter f xs = foldr g a xs
	where
		a = []
		g x rs = if f x then x:rs else rs

miMap f xs = foldr g a xs
	where
		a = []
		g x ls = (f x) : ls

miReverse xs = foldr g a xs
	where
		a = []
		g x ls = ls ++ [x]

unir xs ys = foldr g a xs
	where
		a = ys
		g x ls = x:ls

miTakeWhile f xs = foldr g a xs
	where
		a = []
		g x ls = if f x then x:ls else []

mayor xs = foldr g a xs
	where
		a = head xs
		g x s = if x > s then x else s

ordena xs = foldr g a xs
	where
		a = []
		g x ls = (takeWhile (<=x) ls) ++ (x:dropWhile(<=x)ls)


