------                  EJERCICIO 1                  ------

--1a)
data Carrera = Matematica | Fisica | Computacion | Astronomia | Profesorado deriving (Eq, Show)

--1b)
titulo :: Carrera -> String 
titulo Matematica = "Licenciatura en Matematica" --Decidimos definir cada funcion por separado ya que podemos completar el titulo de Profesorado con "Licenciatura en x"
titulo Fisica = "Licenciatura en Fisica"
titulo Computacion = "Licenciatura en Computacion"
titulo Astronomia = "Licenciatura en Astronomia"
titulo Profesorado = "Profesorado"

--1c) Usando deriving Eq si se puede, podemos comparar x con algo de tipo Carrera
titulo' :: Carrera -> String 
titulo' x | x==Matematica = "Licenciatura en Matematica"
          | x==Fisica = "Licenciatura en Fisica"
          | x==Computacion = "Licenciatura en Computacion"
          | x==Astronomia = "Licenciatura en Astronomia"
          | x==Profesorado = "Profesorado"
          | otherwise = "Es un vago"


------                  EJERCICIO 2                  ------

--2a)
type Ingreso = Int
data Cargo = Titular | Asociado | Adjunto | Asistente | Auxiliar deriving (Eq, Show) --2a
data Area = Administrativa | Ensenanza | Economica | Postgrado deriving (Show,Eq)

data Rol = Decanx
           | Docente Cargo
           | NoDocente Area
           | Estudiante [Carrera] Ingreso
           deriving (Show,Eq)--2f Carrera pasa a ser [Carrera] para poder representar un estudiante que estudia mas de una carrera(y de paso uno que no estudia ninguna)

--2b) Docente :: Cargo -> Rol, Docente es un constructor que toma un parametro Cargo y devuelve el tipo algebraico Rol

--2c)
cuantos_doc :: [Rol] -> Cargo -> Int
cuantos_doc [] _ = 0
cuantos_doc (Docente cargoDoc: xs) cargo | cargoDoc==cargo = 1+cuantos_doc xs cargo
                                         | otherwise = cuantos_doc xs cargo
cuantos_doc (_: xs) cargo = cuantos_doc xs cargo

--2d)
doc_cargo :: Cargo -> Rol -> Bool
doc_cargo cargo (Docente cargoDoc) | cargoDoc==cargo = True
                                   | otherwise = False
doc_cargo _ (_) = False

cuantos_doc' :: [Rol] -> Cargo -> Int --HAY QUE CAMBIARLO Y HACER APLICACION PARCIAL EN LUGAR DE USAR UNA FUNCION ADENTRO DEL FILTER
cuantos_doc' xs cargo = length (filter ((doc_cargo cargo)) xs)

--2e) data Genero = Hombre | Mujer
--    data Rol = Decanx Genero

--2f)
estudia :: Rol -> Carrera -> Bool 
estudia (Estudiante carreras _) carrera = elem carrera carreras --Como matcheamos una lista de carreras?
estudia _ _ = False

------                  EJERCICIO 3                  ------

--3a)
data Persona = Per String String Int Int Int Int Rol deriving (Show,Eq)

--3b) Si se puede

--3c-1)
edad :: Persona -> (Int, Int, Int) -> Int
edad (Per _ _ _ d m a _) (dia, mes, ano) | (m < mes) || (m==mes && (d<=dia)) = ano - a
                                         | otherwise = ano-a-1

--3c-2)
existe :: String -> [Persona] -> Bool
existe _ [] = False
existe apellido ((Per ape _ _ _ _ _ _) : xs) | apellido==ape = True
                                             | otherwise = existe apellido xs

--3c-3)
est_astronomia :: [Persona] -> [Persona]
est_astronomia xs = filter (\(Per _ _ _ _ _ _ rol) -> estudia rol Astronomia) xs

--3c-4)
esNoDocente :: Rol -> Bool
esNoDocente (NoDocente _) = True
esNoDocente _ = False

padron_nodocente :: [Persona] -> [(String,Int)]
padron_nodocente xs = map (\(Per ape nom doc _ _ _ _) -> (ape++" "++nom, doc)) (filter (\(Per _ _ _ _ _ _ rol) -> esNoDocente rol) xs)

------                  EJERCICIO 4                  ------

data Cola = Vacia | Encolada  Persona  Cola deriving (Show,Eq)

--4a-1)
atender :: Cola -> Cola
atender (Vacia) = Vacia
atender (Encolada _ q) = q

--colaTest1 = (Encolada (Per "a" "b" 2 2 2 2 Decanx) (Encolada (Per "a" "b" 2 2 2 2 Decanx) Vacia))

--4a-2)
encolar :: Persona -> Cola -> Cola
encolar p c = Encolada p c

--4a-3)
busca :: Cola -> Cargo -> Persona
busca (Vacia) _ = error "No hay personas en la cola con ese cargo"
busca (Encolada (Per f e d c1 b a (Docente c)) x) q | c==q = (Per f e d c1 b a (Docente c)) --HAY QUE HACER UNA FUNCION P QUE TOMA UNA PERSONA Y DEVUELVE TRUE SI TIENE EL CARGO Q
                                                    | otherwise = busca x q
busca (Encolada (Per _ _ _ _ _ _ _) x) q = busca x q 

--4b)

-- Ver a que tipo se parece

atender' :: [Persona] -> [Persona]
atender' [] = []
atender' (_:xs) = xs

encolar' :: Persona -> [Persona] -> [Persona]
encolar' p xs = xs ++ [p]

busca' :: [Persona] -> Cargo -> Persona
busca' [] _ = error "No hay personas en la fila"
busca' ((Per a b c d e f (Docente x)):xs) cargo | x==cargo = Per a b c d e f (Docente x)
                                                | otherwise = busca' xs cargo
busca' (_:xs) cargo = busca' xs cargo

------                  EJERCICIO 5                  ------

data ListaAsoc a b = Vacia' | Nodo a b (ListaAsoc a b)
type Diccionario = ListaAsoc String String
type Padron = ListaAsoc Int String










