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

promedio' :: [Int] -> Int -- Funcion redondea al mas cercano
promedio' [] = 0
promedio' xs = div (sum xs) (length xs)
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
pertenece n [] = False
pertenece n (x:xs) | x==n = True
		   | otherwise = pertenece n xs

-- Ejercicio 4

encuentra :: Int -> [(Int,String)] -> String
encuentra n [] = ""
encuentra n ((a,b):xs) | a==n = b
		       | otherwise = encuentra n xs

-- Ejercicio 5

paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] t = True
paratodo' (x:xs) t = (t x) && paratodo' xs t

existe' :: [a] -> (a -> Bool) -> Bool
existe' [] t = False
existe' (x:xs) t = (t x) && existe' xs t

sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] t = 0
sumatoria' (x:xs) t = (t x) + sumatoria' xs t

productoria' :: [a] -> (a -> Int) -> Int
productoria' [] t = 1
productoria' (x:xs) t = (t x) * productoria' xs t

-- Ejercicio 6

paratodo'' :: [Bool] -> Bool
paratodo'' xs = paratodo' xs id

-- Ejercicio 7











