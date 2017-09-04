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

promedio :: [Int] -> Int 
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
todosPares xs = paratodo' xs (\x -> (mod x 2 == 0))

hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo n xs = existe' xs (\x -> (mod x n)==0)

sumaCuadrados :: Int -> Int
sumaCuadrados n = sumatoria' [0..n] (\x -> x*x) 

soloPares :: [Int] -> [Int]
soloPares [] = []
soloPares (x:xs) | (mod x 2)==0 = x:soloPares xs
                 | otherwise = soloPares xs

multiplicaPares :: [Int] -> Int
multiplicaPares xs = productoria' (soloPares xs) id 

-- Ejercicio 8
-- map xs f: Es una funcion que devuelve una lista formada por todos los elementos de la lista xs al aplicar f a cada uno
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


mayoresA :: Ord a => a -> [a] -> [a] 
mayoresA _ [] = []
mayoresA n (x:xs) | x>n = x:(mayoresA n xs)
                  | otherwise = mayoresA n xs

mayoresA' :: Ord a => a -> [a] -> [a]
mayoresA' n xs = filter (\x -> x>n) xs

-- Ejercicio 12

encuentra' :: Int -> [(Int,String)] -> String
encuentra' n xs = case (filter (\(a,_) -> a==n) xs) of 
                        [] -> ""
                        (x:_) -> snd x

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
minimo [] = undefined
minimo (x:[]) = x
minimo (x:xs) = min x (minimo xs)

minimo' :: Ord a => Bounded a => [a] -> a
minimo' [] = maxBound
minimo' (x:[]) = x
minimo' (x:xs) = min x (minimo xs)

-- Ejercicios Estrella

--1

-- a) Esta bien tipado y cubre todos los casos de definición. (a,b) es una tupla y tanto a como b pueden tomar cualquier tipo. x es una tupla (a y b pueden ser de cualquier tipo).
-- b) Esta bien tipado pero no cumple con todos los casos, en particular el caso vacío. En este caso x debe tener el mismo tipo que a, e y el mismo tipo que b (a y b pueden ser de cualquier tipo).
-- c) No esta bien tipado. (a,b) es una tupla, no una lista de tuplas.
-- d) Esta bien tipado pero el caso vacío no esta definido ya que (x:xs) necesita obtener al menos un elemento. x es una tupla donde el primer componente es del mismo tipo que a y el segundo componente es del mismo tipo que b (a y b pueden ser de cualquier tipo).
-- e) Esta bien tipado pero no esta definido para todos los casos ya que necesita obtener al menos dos elementos que deben ser tuplas.
-- f) Esta bien tipado pero no cumple con todos los casos ya que la lista tiene que tener una sola tupla donde el primer elemento es 0. No cumple para la lista vacia o cuando tiene mas de una tupla. a puede tomar cualquier tipo.
-- g) Esta bien tipado pero no cumple para la lista vacía o cuando el segundo elemento de la tupla no es de tipo Num==1. x es de tipo Int.
-- h) Esta bien tipado pero no cumple para la lista vacía o cuando el primer elemento de la tupla es distinto de 1. x es del mismo tipo de a donde a puede ser de cualquier tipo.
-- i) Esta bien tipado y cumple con todos los casos. a es una funcion que toma un Int y devuelde otro Int y b es un Int.
-- j) Esta bien tipado pero no cumple con los casos donde el segundo elemento es distinto de 3. a es una funcion que toma un Int y devuelde otro Int.
-- k) No esta bien tipado porque esta tomando tres argumentos en vez de una funcion que toma un argumento y devuelve otro y otro argumento.
-- l) Esta bien tipado y cumple todos los casos. a es de cualquier tipo y g es una funcion que toma a y devuelve a.

-- 2

-- a) f x = fst x
-- b) f x = snd x 
-- c) f (a,b) = typeOf (snd x)
--    f' (a,b) = typeOf (fst x)  
-- d) f x = typeOf x 
-- e) f x y = ($!) x y , f' x y = ($) x y  
-- f) f x y = map x y
-- g) f x y = typeOf (x (y)) --no nos cierra
-- h) f x y z = (.) x y z 






