data Natural = Cero | Sgte Natural
               deriving Show
--1) Definir (+) suma sobre el tipo Natural
a=Cero--0
b=(Sgte Cero)--1
c=(Sgte (Sgte Cero))--2
d=(Sgte(Sgte (Sgte Cero)))--3
e=(Sgte (Sgte(Sgte (Sgte Cero))))--4

suma :: Natural -> Natural -> Natural
suma n Cero = n
suma n (Sgte m) = Sgte (suma n m)

convertirNatural :: Natural -> Int
convertirNatural Cero = 0
convertirNatural (Sgte n) = 1 + (convertirNatural n)

aNumero :: Int -> Natural
aNumero 0 = Cero
aNumero n = Sgte (aNumero (n-1))
sumas = convertirNatural (suma (aNumero 3)(aNumero 4))

--2) Definir producto, potencia, resta, mayor,menor,residuo, ==; 
(Trabajar con 2 numeros)

producto :: Natural -> Natural -> Natural
producto n Cero = Cero
producto Cero (Sgte m) = Cero
producto n (Sgte m) = suma n (producto n m)

resta :: Natural -> Natural -> Natural
resta Cero Cero = Cero
resta n Cero = n
resta Cero m = m
resta (Sgte n) (Sgte m) = resta n m

potencia :: Natural-> Natural -> Natural
potencia Cero m = Cero
potencia n Cero = (Sgte Cero)
potencia n (Sgte m) = producto n (potencia n m)

mayor :: Natural-> Natural -> Bool
mayor Cero Cero = False
mayor n Cero = True
mayor Cero m = False
mayor (Sgte n) (Sgte m) = mayor n m

menor ::Natural-> Natural -> Bool
menor Cero Cero = False
menor n Cero = False
menor Cero m = True
menor (Sgte n) (Sgte m) = menor  n m

igual :: Natural-> Natural -> Bool
igual Cero Cero = True
igual n Cero = False
igual Cero m = False
igual (Sgte n) (Sgte m) = igual n m
