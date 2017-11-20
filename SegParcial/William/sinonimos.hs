l1=[Lun, Jue, Vie, Sab , Dom]
--data Bool = True|False

data Shape = Circle Float Float Float | Rectangule  Float Float Float Float

area3d :: Shape -> Float
area3d (Circle _ _ r) = pi * r*r

--dias de desacanso
data Dia= Lun|Mar|Mie|Jue|Vie|Sab|Dom
        deriving Show
esdescanso :: Dia ->Bool
esdescanso Dom = True
esdescanso Sab = True
esdescanso _ = False

--funcion q reciba un dia y retorne el sgte
diasgte::Dia ->Dia
diasgte Lun=Mar
diasgte Mar = Mie
diasgte Mie = Jue
diasgte Jue = Vie
diasgte Vie = Sab
diasgte Sab = Dom
diasgte Dom = Lun

--dados dos dias retorne el mayor de los 2
diamay :: Dia ->Dia -> Dia
diamay d1 d2 = if num d1 > num d2
  then d1
  else d2
  where
    num Lun = 1
    num Mar = 2
    num Mie = 3
    num Jue = 4
    num Vie = 5
    num Sab = 6
    num Dom = 7

--3) Definir una funcion que reciba una lista de dias y devuelva una lista de los dias que no son de descanso
diasNodes :: [Dia]->[Dia]
diasNodes [] = []
diasNodes (x:xs) = if (esdescanso x) ==True
  then x:diasNodes xs
  else diasNodes xs

-- Definir una funcion que reciba un dia "d" y un # "n" y devuelva el dia que resulta de sumar "n" dias a "d"
diasdes :: Dia->Integer ->Dia
diasdes d 0 = d
diasdes d n = diasdes (diasgte d) (n-1)

--Definir un tipo de dato para definir los mese del año
data Mes = Ene|Feb|Marz|Abr|May|Jun|Jul|Ago|Sep|Oct|Nov|Dic
  deriving (Show)

--Utilizar este tipo de datos para definir:
--a) Una Funcion que reciba un mes y devuelva la cantidad de dias que normalmente tiene ese mes

diasdemes :: Mes->Int
diasdemes Feb = 28
diasdemes Sep = 30
diasdemes Nov = 30
diasdemes Jun = 30
diasdemes Abr = 30
diasdemes _ = 31

--b) Definir una funcion que reciba un mes y devuelva un mensaje del feriado tipico de ese mes
ferdmes :: Mes -> String
ferdmes Ene = "Fundacion del estado Plurinacional -_- bueno aunque sea es feriado "
ferdmes Feb = "27 28 carnaval "
ferdmes Marz = "Dia del padre no es feriado por desgraci"
ferdmes Abr = "dia del niño viernes santo para salia a beber"
ferdmes May = "dia del trabajador feriado y de la madre"
ferdmes Jun = "Año nuevo aymara no hay clases"
ferdmes Jul = "No voy a clases mi cumpleaños "
ferdmes Ago = "independencia de bolivia "
ferdmes Sep = "Aniversario de cbba"
ferdmes Oct = "Aqui no hay ni madres"
ferdmes Nov = "Dia de los muertos y de las aulas vacias "
ferdmes Dic = "Año nuevo navidad para los padres "

--c)Definir una funcion que reciba 2 meses y devuelva a cantidad de meses transcurridos entre ambos
mestrans :: Mes ->Mes ->Int
mestrans m1 m2 = if num m1 > num m2 then (num m1) - (num m2) else (num m2)-(num m1)
    where
      num Ene = 1
      num Feb = 2
      num Marz = 3
      num Abr = 4
      num May = 5
      num Jun = 6
      num Jul = 7
      num Ago = 8
      num Sep = 9
      num Oct = 10
      num Nov = 11
      num Dic = 12

numm :: Mes -> Int
numm Ene = 1
numm Feb = 2
numm Marz = 3
numm Abr = 4
numm May = 5
numm Jun = 6
numm Jul = 7
numm Ago = 8
numm Sep = 9
numm Oct = 10
numm Nov = 11
numm Dic = 12

messgte ::Mes ->Mes
messgte Ene = Feb
messgte Feb  =Marz
messgte Marz =Abr
messgte Abr = May
messgte May = Jun
messgte Jun = Jul
messgte Jul = Ago
messgte Ago = Sep
messgte Sep = Oct
messgte Oct = Nov
messgte Nov = Dic
messgte Dic = Ene

mesante::Mes-> Mes
mesante Ene = Dic
mesante Feb = Ene
mesante Marz= Feb
mesante Abr = Marz
mesante May = Abr
mesante Jun = May
mesante Jul = Jun
mesante Ago = Jul
mesante Sep = Ago
mesante Oct = Sep
mesante Nov = Oct
mesante Dic = Nov

--d) Definir una funcion que reciba 2 meses y devuelva la cantidad de dias transcurridos entre ambos
diasmtrans:: Mes -> Mes -> Int
diasmtrans m1 m2 |numm m1 == numm m2 = diasdemes m1
  | numm m1 < numm m2 = diasdemes m1 + (diasmtrans (messgte m1) m2)
  | numm m2 < numm m1 = diasdemes m1 + (diasmtrans (mesante m1) m2)
