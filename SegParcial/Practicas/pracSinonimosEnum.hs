-- I .
data ZonaGeografica = Valles | Llanos | Altiplano
	deriving Show
data Departamento = LaPaz | Oruro | Potosi | Cochabamba | Sucre | Tarija | SantaCruz | Pando | Beni
	deriving Show
-- 1.
caract :: ZonaGeografica -> String
caract Valles = "Zona templada"
caract Llanos = "Zona Calurosa y tropical"
caract Altiplano = "Zona frigida y Arida"

-- 2.
esValle :: Departamento -> Bool
esValle Cochabamba = True
esValle Sucre = True
esValle Tarija = True
esValle _ = False

-- 3.
zona :: Departamento -> ZonaGeografica
zona LaPaz = Altiplano
zona Oruro = Altiplano
zona Potosi = Altiplano
zona Cochabamba = Valles
zona Sucre = Valles
zona Tarija = Valles
zona _ = Llanos

-- 4.
dep = [LaPaz, Oruro, Potosi, Cochabamba, Sucre, Tarija, SantaCruz, Pando, Beni] 

esLlano :: Departamento -> Bool
esLlano SantaCruz = True
esLlano Pando = True
esLlano Beni = True
esLlano _ = False

llanosValles :: [Departamento] -> ([Departamento], [Departamento])
llanosValles ds = escoje ds ([], [])
escoje [] (lls, vll) = (lls, vll)
escoje (d:ds) (lls, vll) | esValle d = escoje ds (lls, d:vll)
              			 | esLlano d = escoje ds (d:lls, vll)
             			 | otherwise = escoje ds (lls, vll)

-- II.
type Dia = Integer
type Mes = Integer
type Anio = Integer
type Fecha = (Dia, Mes, Anio)
type Periodo = (Fecha, Fecha)
type Nombre = String
type Presidente = (Nombre, Periodo)

-- 1.
p1 = ((30,4,1993), (4, 5, 1997))

getFecha1 (f1, _) = f1
getFecha2 (_, f2) = f2
getAnio (_, _, a) = a

aniosTrans :: Periodo -> Anio
aniosTrans per = abs(a1 - a2)
	where
		a1 = getAnio(getFecha1 per)
		a2 = getAnio(getFecha2 per)

-- 2.
pres1 = ("Evo", ((30,4,1993), (4, 5, 1997)))
pres2 = ("Mesa", ((30,4,1997), (4, 5, 1999)))
pres3 = ("Gallo", ((30,4,2000), (4, 5, 2001)))
pres4 = ("Goni", ((30,4,2002), (4, 5, 2011)))
pres5 = ("Banzer", ((30,4,2011), (4, 5, 2020)))

getPeriodo ( _, p) = p 

goberno :: Presidente -> Anio 
goberno p = aniosTrans(getPeriodo p)

-- 3.
masTiempo :: Presidente -> Presidente -> Presidente
masTiempo pr1 pr2 = if c1 > c2 then pr1 else pr2
	where
		c1 = aniosTrans (getPeriodo pr1)
		c2 = aniosTrans (getPeriodo pr2)

-- 4.
lp = [pres1, pres2, pres3, pres4, pres5]
getNombre (n, _) = n

menosTiempo :: [Presidente] -> Nombre
menosTiempo ps = getNombre(buscaMenor ps)
buscaMenor [p] = p
buscaMenor (p:ps) = menor p (buscaMenor ps)
menor p1 p2 | (aniosTrans (getPeriodo p1)) > (aniosTrans (getPeriodo p2)) = p2
			| otherwise = p1

-- 5. 
antesDelNov :: [Presidente] -> [Nombre]
antesDelNov ps = buscador ps []
buscador [] adn = adn
buscador (p:ps) adn = if cumple p then buscador ps ((getNombre p):adn) else buscador ps adn
	where 
		cumple p = (getAnio (getFecha1 (getPeriodo p))) < 1990

-- 6.
menosCuatroAnios :: [Presidente] -> Int
menosCuatroAnios ps = buscaPer ps 0
buscaPer [] c = c
buscaPer (p:ps) c = if cumpleCuatroA p then buscaPer ps (c+1) else buscaPer ps c
	where cumpleCuatroA p = (aniosTrans (getPeriodo p)) < 4 

-- 7.
ordena ps = quick ps
quick (p:ps) = quick (menores p ps) ++ [p] ++ quick(mayores p ps)

menores p [] = []
menores p (op:ops) = if 

fechaMayorD :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
fechaMayorD f1@(d1, m1, a1) f2@(d2, m2, a2)
        | a1 /= a2 = may a1 a2
        | m1 /= m2 = may m1 m2
        | otherwise = may d1 d2
        where
