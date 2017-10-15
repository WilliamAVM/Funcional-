--3
logicaAB :: Bool -> Bool -> Int
logicaAB a b = case (a&&b) of True->1
                              False->0
 
xor :: Bool -> Bool -> Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False

logicaXOR a b = case (a `xor` b) of True->1
                                    False->0
 
logicaXORAB a b = case ( (a&& (not b)) || (b&&(not a))  ) of True->1
                                                             False->0

--11
sumatoria :: Int -> Int -> Int -> String
sumatoria a b c = case ((a+b+c)`div`10) of 1 -> "Sumatoria mayor"
                                           0 -> "Sumatoria menor"
                                           otherwise -> "Vacio"