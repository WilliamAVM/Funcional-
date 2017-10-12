--1.- funcion q reciba 1 vocal y retorne el sgte
vocalC :: Char->Char

vocalC v = case v of
  'a'->'e'
  'e'->'i'
  'i'->'o'
  'o'->'u'
  'u'->'a'

--2.-recibe un digito y retorna su literal
literalC :: Int->String
literalC n = case n of
  1 -> "uno"
  2 -> "dos"
  3 -> "tres"
  4 -> "cuatro"
  5 -> "cinco"
  6 -> "seis"
  7 -> "siete"
  8 -> "ocho"
  9 -> "nueve"
  0 -> "cero"

--3.- dado dos valores q representan true 1 y false 0 aplicar la operacion AND
funANDC :: Bool->Bool->Bool
funANDC a b = case a of
   False -> False
   True -> case b of
     True -> True
     False -> False
--4.-identico al anterior pero con OR
funORC ::Bool->Bool->Bool
funORC a b  = case a of
  True -> case b of
   False -> True
   True-> True
  False -> case b of
   True -> True
   False -> False

--5.-identico al anterior pero con XOR
funXORC :: Int->Int->Int --fdf->Bool
funXORC a b  = case a of
  0 -> case b of
    0 -> 0
    1 -> 1
  1-> case b of
    0 -> 1
    1 -> 0
  otherwise -> a-b

--6.- identico al anterior pero q reciva el operador y apliq dependiendo de cual sea
funXOAC :: String -> Bool -> Bool -> Bool
funXOAC f a b = case f of
  "AND" -> (funANDC a b)
  "OR" -> (funORC a b)
  --"XOR" -> funXORC a b

--7.- recibir un # de 2 cifra y devolver su literal
funLit2Cif:: Int-> String
funLit2Cif a = case a of
  10 -> "diez"
  11 -> "once"
  12 -> "doce"
  13 -> "trece"
  14 -> "catorce"
  15  -> "quince"
  --16 -> "diesi":literalC(10-n)
  --s17 -> "diesi":literalC(10-n)
  otherwise -> "expression"

--9.- reciba dos # y retorne el menor
menorC :: Int->Int->Int
menorC a b = case a of
  a-> a `min` b
--10.- reciba 6 # y retorne el menor
menor6C :: Int->Int->Int->Int->Int->Int->Int
menor6C a b c d e f = case a of
 a -> (menorC(menorC a b) (menorC (menorC c d) (menorC e f)))

 --11.- funcion q reciba 3 # y retorne mensaje "sumatoria mayor" sila sumatoria es menor a 20 "sumatoria mwnor"
 -- sila sumatoria es menor a 10 y en otro caso "vacio"
summay x y z = if ((x+y+z)/3) < 21 && ((x+y+z)/3)>10
   then "Sumatoria Mayor"
   else if ((x+y+z)/3)<11 && ((x+y+z)/3)>0
       then "Sumatoria Menor"
       else "Vacio"

funSum3 :: Double->Double->Double->String
funSum3 a b c = case a of
  a -> (summay a b c)
  --otherwise -> "expression"

--11.- funcion q reciba 3 notas y retorne mensaje "Excelente" sila sumatoria es esta entre 90-100 "bien"
--si esta entre 70-89 regular si esta entre 51-69 y mal si esta entre 0-50
{-
prom3notas :: Int->Int->Int->String
prom3notas a b c= let d ((a+b+c)/3) case d of
 -}
 
