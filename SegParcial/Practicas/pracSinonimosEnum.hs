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
pres1 = ("Evo", ((30,4,1993), (28, 4, 1997)))
pres2 = ("Mesa", ((30,4,1997), (4, 5, 1999)))
pres3 = ("Gallo", ((30,6,2000), (4, 5, 2001)))
pres4 = ("Goni", ((30,4,2002), (4, 3, 2011)))
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
ordena :: [Presidente] -> [Presidente]
ordena ps = quick ps
quick [] = []
quick (p:ps) = quick (menores p ps) ++ [p] ++ quick(mayores p ps)
menores p [] = []
menores p (l:ls) = if esMenor (getFecha2(getPeriodo (l))) (getFecha1(getPeriodo (p))) then
						l:(menores p ls) else (menores p ls)
mayores p [] = []
mayores p (l:ls) = if esMayor (getFecha1(getPeriodo (l))) (getFecha2(getPeriodo (p))) then
						l:(mayores p ls) else (mayores p ls)
esMayor (d1, m1, a1) (d2, m2, a2)| a1 /= a2 = (a1 > a2)
							   	 | m1 /= m2 = (m1 > m2)
							     | otherwise = (d1 > d2)
esMenor (d1, m1, a1) (d2, m2, a2)| a1 /= a2 = (a1 < a2)
							   	 | m1 /= m2 = (m1 < m2)
							     | otherwise = (d1 < d2)

-- III.
data Empleado = Docente Name Horas SueldoHra Materias | Administrativo Name Salario Cargo
	deriving Show

type Name      = String
type Horas     = Int
type SueldoHra = Int
type Salario   = Int
type Materias  = [String]
type Cargo     = String 

--a)
d1 = Docente "Jorge" 100 20 ["logica", "funcional"]
d2 = Administrativo "Nelson" 2100 "Mensajero"
d3 = Docente "Adrian" 100 22 ["calculo1", "ecuaciones"]
d4 = Docente "Daniel" 100 24 ["matdiscreta"]
d5 = Docente "Hernan" 100 26 ["programacion", "sis1", "sis2"]
d6 = Docente "Ariel" 100 29 ["metodologia", "orga"]

emp = [d1, d2, d3, d4, d5, d6]

mismoIngMensual :: Empleado -> Empleado -> Bool
mismoIngMensual e1 e2 = (getSueldo e1) == (getSueldo e2)
getSueldo (Docente _ h sh _) = h * sh
getSueldo (Administrativo _ s _) = s  

--b)
nameDocMasMaterias :: [Empleado] -> Name
nameDocMasMaterias em = getName (docMasMaterias (filter esDocente em)) 
docMasMaterias [em] = em
docMasMaterias (em:ems) = maxim em (docMasMaterias ems) 
maxim d1 d2 = if getCantMat d1 > getCantMat d2 then d1 else d2

getName (Docente n _ _ _) = n
getName (Administrativo n _ _) = n
getCantMat (Docente _ _ _ ms) = length ms

esDocente (Docente _ _ _ _)= True
esDocente (_) = False

--c)
nameEmpMasSueldo :: [Empleado] -> Name
nameEmpMasSueldo  em = getName (empMasSueldo em)
empMasSueldo [em] = em
empMasSueldo (em:ems) = maxSueldo em (empMasSueldo ems)
maxSueldo e1 e2 = if getSueldo e1 > getSueldo e2 then e1 else e2

--d)
admin = Administrativo "Luis" 2300 "Portero"
insertaEmp :: Empleado -> [Empleado] -> [Empleado]
insertaEmp em [e] | (getSueldo em) > (getSueldo e) = ([e]++[em])
				  | otherwise = ([em]++[e])
insertaEmp em (x:y:ems) | (getSueldo em) > (getSueldo x) && (getSueldo em) < (getSueldo y) = (x:em:y:ems)
						| (getSueldo em) < (getSueldo x) && (getSueldo em) < (getSueldo y) = (em:x:y:ems)
						| otherwise = x:(insertaEmp em (y:ems))

--e)
ordenar :: [Empleado] -> [Empleado]
ordenar ems = quick1 ems
quick1 [] = []
quick1 (e:em) = quick1(menores1 e em)
menores1 e [] = []
menores1 e (l:ls) | (getSueldo l) < (getSueldo e) = l : menores1 e ls
				  | otherwise  = menores1 e ls
mayores1 e [] = []
mayores1 e (l:ls) | (getSueldo l) > (getSueldo e) = l : mayores1 e ls
				  | otherwise = mayores1 e ls