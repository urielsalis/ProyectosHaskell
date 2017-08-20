-- Ejercicio 1

esCero :: Int -> Bool
esCero x = x==0

esPositivo :: Int -> Bool
esPositivo x = x>0

esVocal :: Char -> Bool
esVocal x = x=='a' || x=='e' || x=='i' || x=='o' || x=='u'

factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x-1)

promedio :: [Int] -> Int -- Funcion redondea siempre abajo
promedio [] = 0
promedio (x:xs) = (x + sum xs) `div` (1 + length xs)

-- Ejercicio 2

paratodo :: [Bool] -> Bool
paratodo [] = True
paratodo (x:xs) | x = paratodo xs
                | otherwise = False

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

-- Ejercicio 3

pertenece :: Int -> [Int] -> Bool
pertenece _ [] = False
pertenece n (x:xs) | x==n = True
                   | otherwise = pertenece n xs

-- Ejercicio 4

encuentra :: Int -> [(Int,String)] -> String
encuentra _ [] = ""
encuentra n ((a,b):xs) | a==n = b
                       | otherwise = encuentra n xs

-- Ejercicio 5

paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] _ = True
paratodo' (x:xs) t = (t x) && paratodo' xs t

existe' :: [a] -> (a -> Bool) -> Bool
existe' [] _ = False
existe' (x:xs) t = (t x) || existe' xs t

sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] _ = 0
sumatoria' (x:xs) t = (t x) + sumatoria' xs t

productoria' :: [a] -> (a -> Int) -> Int
productoria' [] _ = 1
productoria' (x:xs) t = (t x) * productoria' xs t

-- Ejercicio 6

paratodo'' :: [Bool] -> Bool
paratodo'' xs = paratodo' xs id

-- Ejercicio 7
todosPares :: [Int] -> Bool
todosPares xs = paratodo' xs (\x -> (mod x 2 == 0)) --Es legal usar funciones anonimas o tenemos que usar esPar?

hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo n xs = existe' xs (\x -> (mod x n)==0)

sumaCuadrados :: Int -> Int
sumaCuadrados n = sumatoria' [0..n] (\x -> x*x) --  (^2) da una warning

multiplicaPares :: [Int] -> Int
multiplicaPares xs = productoria' ([ x | x <- xs, (mod x 2)==0]) id --Se puede usar una funcion auxiliar definida por recursion que haga que el caso base sea 0?

-- Ejercicio 8
-- map xs f: Es una funcion que devuelve una lista formada por todos los elementos de la lista xs al aplicar p a cada uno
-- filter xs f: Es una funcion que devuelve una lista formada por todos los elementos de la lista xs que cumplen f
-- map succ [1, -4, 6, 2, -8] equivale a aplicar succ a cada elemento de la lista, por lo que el resultado es [2, -3, 7, 3, -7]
-- filter esPositivo [1, -4, 6, 2, -8] equivale a devolver una lista formado por los items de la lista que cumplen esPositivo, por lo tanto el resultado es [1, 6, 2]

-- Ejercicio 9
duplicar :: [Int] -> [Int] --9a
duplicar [] = []
duplicar (x:xs) = (x*2):duplicar xs

duplicar' :: [Int] -> [Int] --9b
duplicar' xs = map (*2) xs

-- Ejercicio 10

sonPares :: [Int] -> [Int] --10a
sonPares [] = []
sonPares (x:xs) | (mod x 2)==0 = x:sonPares xs
                | otherwise = sonPares xs

sonPares' :: [Int] -> [Int] --10b
sonPares' xs = filter (\x -> (mod x 2)==0) xs

multiplicaPares' :: [Int] -> Int --10c
multiplicaPares' xs = productoria' (filter (\x -> (mod x 2)==0) xs) id

-- Ejercicio 11
sumarALista :: Num a => a -> [a] -> [a]
sumarALista _ [] = []
sumarALista n (x:xs) = (n+x):sumarALista n xs

sumarALista' :: Num a => a -> [a] -> [a]
sumarALista' n xs = map (\x -> x+n) xs 


encabezar :: a -> [[a]] ->[[a]]
encabezar _ [] = []
encabezar n [[]] = [[n]]
encabezar n [xs] = [(n:xs)]
encabezar n (xs:xss) = (n:xs) : (encabezar n xss)

encabezar' :: a -> [[a]] ->[[a]]
encabezar' n xss = map (\xs -> n:xs) xss 


mayoresA :: Ord a => a -> [a] -> [a] --Esta mal en el cuadernillo, tiene que ser Ord a => en vez de Ord a ->
mayoresA _ [] = []
mayoresA n (x:xs) | x>n = x:(mayoresA n xs)
                  | otherwise = mayoresA n xs

mayoresA' :: Ord a => a -> [a] -> [a]
mayoresA' n xs = filter (\x -> x>n) xs

-- Ejercicio 12
obtenerB :: [(Int, String)] -> String
obtenerB [] = []
obtenerB [(_, b)] = b
obtenerB ((_,b):_) = b  

encuentra' :: Int -> [(Int,String)] -> String
encuentra' n xs = obtenerB (filter (\(a,_) -> n==a) xs)

-- Ejercicio 13
primIgualesA :: Eq a => a -> [a] -> [a]
primIgualesA _ [] = []
primIgualesA n (x:xs) | x==n = x:primIgualesA n xs
                      | otherwise = []

primIgualesA' :: Eq a => a -> [a] -> [a]
primIgualesA' n xs = takeWhile (\x -> x==n) xs 

-- Ejercicio 14
primIguales :: Eq a => [a] -> [a]
primIguales [] = []
primIguales (x:[]) = [x]
primIguales (x:xs) | x==head xs = x:primIguales xs
                   | otherwise = [x]

primIguales' :: Eq a => [a] -> [a]
primIguales' xs = primIgualesA (head xs) xs

-- Ejercicio 15
minimo :: Ord a => [a] -> a
minimo (x:[]) = x
minimo (x:xs) = min x (minimo xs)

minimo' :: Ord a => Bounded a => [a] -> a
minimo' [] = error "Lista vacia" --Cual podria ser un minimo?? 
minimo' (x:[]) = x
minimo' (x:xs) = min x (minimo xs)
