l1 = [1,2, 3, 4, 5, 6, 7, 8,9]
l2 = reverse l1
l3 = [[1,2],[3,4]]
l4 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
miFilter :: (tx -> Bool) -> [tx] -> [tx]
miFilter f xs = [x | x <- xs, f x]

miMap :: (tx -> tf) -> [tx] -> [tf]
miMap f xs = [f x | x <- xs]

miLength :: [tx] -> Int
miLength xs = sum[1 | x <- xs]

miComp :: Eq tx => [tx] -> [tx] -> Bool
miComp xs ys = if l1 /= l2 then False else and([f x | x <- [0..(l1-1)]])
    where l1 = length xs
          l2 = length ys
          f x = ((!!) xs x) == ((!!) ys x)

insert1 :: [tx] -> tx -> [tx]
insert1 xs y = y : [x | x <- xs]

insert2 :: [tx] -> tx -> [tx]
insert2 xs y = [x | x <- xs] ++ [y]

ordena [] =[]
ordena (x:xs)=
  let menoresOrdenados = ordena [a|a<-xs, a <=x]
      mayoresordenados = ordena [a|a<-xs, a>x]
  in  menoresOrdenados ++ [x] ++ mayoresordenados

verOrden :: Ord x => [x] -> Bool
verOrden xs = and([f x | x <- [0..(length ls-1)]])
    where ls = tail xs
          f i = ((!!) xs i) <= ((!!) ls i)

transpuesta [] = []
transpuesta ([]:xss) = transpuesta xss
transpuesta ((x:xs):xss) = (x:[h|(h:_)<-xss]): transpuesta(xs:[t|(_:t)<-xss])





















--fun a = if then else

--ordenAcs xs = [ordenar x1 x2| x1 <-xs,x2<-xs ]
--sacar2 xs =
--ordenar x y = if x> y then [y,x] else [x,y]
--contador xs = [x|x<-[0..(length(xs)-1)]]

{--transMat xss = if f1==f2 then [] else [1]
  where f1= length ((!!) xss 0)
        f1= length ((!!) xss 1)
--[(x) |x <- [0..(length xss-1)] ]


--transMat xss = [f x | x <- [0..(length xss-1)]]
--    where f i = ()
-}
