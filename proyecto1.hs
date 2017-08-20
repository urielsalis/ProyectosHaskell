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
existe' (x:xs) t = (t x) && existe' xs t

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

esPar :: Int -> Bool -- Se puede usar esto?
esPar x = mod x 2 == 0
cuadrado :: Int -> Int
cuadrado i = i^2

todosPares :: [Int] -> Bool
todosPares xs = paratodo' xs (\x -> (mod x 2 == 0)) --Es legal usar funciones anonimas o tenemos que usar esPar?

esMultiplo :: Int -> Int -> Bool -- Se puede usar esto?
esMultiplo n x = mod x n == 0
hayMultiplo :: Int -> [Int] -> Bool --Porque no anda estooooooooooooooooo
hayMultiplo n xs = existe' xs (esMultiplo n)

sumaCuadrados :: Int -> Int
sumaCuadrados n = sumatoria' [0..n] (^2)

multiplicaPares :: [Int] -> Int
multiplicaPares xs = productoria' (filter esPar xs) id --Se puede usar filter?

-- Ejercicio 8
-- map: 