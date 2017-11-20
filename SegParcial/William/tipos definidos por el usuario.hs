--Definir una funcion que reciba una nota y devuelva el mensaje aprobado o reprobado segun el caso. La nota
-- puede ser de pregrado en cuyo caso podra ser un valor entre 0 y 100 o de postgrado, cuyo caso sera un valor
-- entre A,B,C y D.
--Una nota de pregrado es de aprobacion si es mayor a 50 y una nota de postgrado es de aprobacion si es
--diferente de D
--data Nota = A|B|C|D|Cero|Uno|Dos|Tres|.......|Ci
data Nota = Pregrado Int | Postgrado Char
  deriving (Show)

nota:: Nota -> String
nota (Pregrado n)|n>=51 && n<=100 = "Aprobado"
  | otherwise = "reprobado"
nota (Postgrado n)| n /= 'D'= "aprobado"
  | otherwise = "reprobado"


--1) Definir una funcion que reciba una lista de notas y devuelva una lista con solamente las notas de
-- aprobacion de pregrado
p1=[Pregrado 51, Pregrado 100]
notasPre :: [Nota] ->[Nota]
notasPre [] = []
notasPre (x:xs) = if esAprobadoPre x
  then (x:notasPre xs)
  else (notasPre xs)

esAprobadoPre :: Nota -> Bool
esAprobadoPre (Pregrado x) = x>50
esAprobadoPre _ = False

--2) Definir una funcion que reciba 2 notas y devuelva la nota mayor. Suponer que toda nota de postgrado
--es mayor a cualquier nota de pregrado
esmay:: Nota -> Nota -> Nota
esmay (Postgrado n1 ) (Pregrado n2) = (Postgrado n1)
esmay (Pregrado n1) (Postgrado n2) = (Postgrado n2)
esmay (Pregrado n1) ((Pregrado n2)) = if n1 >n2
  then (Pregrado n2)
  else (Pregrado n1)
esmay (Postgrado n1) (Postgrado n2) = if n1 =='A' && n2 /='A'then (Postgrado n1)
  else (Postgrado n2)
esmay (Postgrado n1) (Postgrado n2) = if n1 =='B' && n2 /='A' || n2 /='B'then (Postgrado n1)
  else (Postgrado n2)
esmay (Postgrado n1) (Postgrado n2) = if n1 =='C' && n2 /='A' || n2 /='B' || n2 /='C'then (Postgrado n1)
  else (Postgrado n2)
--se q esta feo pero q flojera pensar algo mas eficas y eficiente

--3) Definir una funcion que reciba una lista de notas y devuelva la nota mayor
mayno ::[Nota]->Nota
mayno [] =(Pregrado 0)
mayno (x:xs) =(esmay x (mayno xs))

--5) Definir una funcion que reciba dos numeros y devuelva el mayor en caso de que los numeros sean iguales
-- devolver el mensaje "Numeros iguales"

data Respuesta = Ok Int  | Inesperado String
  deriving (Show)

may' ::Int->Int-> Respuesta
may' a b | a> b = Ok a
  | a < b = Ok b
  | otherwise = Inesperado "Numeros Iguales alaverga"


--6) Definir una funcionque reciba una nota (0,100) y devuelva True si es nota de aceptacion,
-- False si no. Si la nota es invalida que retorne "Nota Invalida"
data Resp = Ok1 Bool | Inespera String
  deriving (Show)
notaAcep :: Int ->Resp
notaAcep n = if (n >=0) && (n<=100) && (n>=51)
  then Ok1 True
  else
    if (n >=0) && (n<=100) && (n<51)
      then Ok1 False
      else Inespera "Nota Invalida"
--7) Definir una funcion que reciba una vocal y devuelva su siguiente. Si el caracter ingresado no es una
-- vocal que devuelva "No es vocal"
data Respu = A Char | B String
  deriving (Show)
esvocal :: Char -> Respu
esvocal 'a'= A 'e'
esvocal 'e'= A 'i'
esvocal 'i'= A 'o'
esvocal 'o'= A 'u'
esvocal 'u'= A 'a'
esvocal _ = B "No esvocal"
