module Language.ListaAsoc where

data ListaAsoc a b = Vacia
                   | Nodo a b (ListaAsoc a b) deriving (Eq,Show)
                   
				   
-- Devuelve la cantidad de "datos" en una lista				   
la_long :: Integral c => ListaAsoc a b -> c
la_long (Vacia) = 0
la_long (Nodo _ _ xs) = 1 + la_long xs


-- Transforma una lista de asociaciones en una lista de pares (clave, dato)
la_pares :: ListaAsoc a b -> [(a,b)]
la_pares (Vacia) = []
la_pares (Nodo a b xs) = (a,b) : la_pares xs


-- Dada una lista y una clave devuelve el dato asociado, si es que existe. En caso contrario devuelve Nothing.
la_buscar :: Eq a => ListaAsoc a b -> a -> Maybe b
la_buscar (Vacia) _ = Nothing
la_buscar (Nodo a b xs) x | x==a = Just b
                          | otherwise = la_buscar xs x


-- Verifica si una clave existe en una lista
la_existe :: Eq a => ListaAsoc a b -> a -> Bool
la_existe (Vacia) _ = False
la_existe (Nodo c _ xs) x | x==c = True
                          | otherwise = la_existe xs x


-- Dada una lista, agrega una clave con su valor asociado "d" en la lista si la clave NO existe. En caso de que la clave exista, reemplaza el dato con el nuevo dato "d".
la_agregar :: Eq a => a -> b -> ListaAsoc a b -> ListaAsoc a b
la_agregar c d (Vacia) = (Nodo c d (Vacia))
la_agregar c d (Nodo a b xs) | c==a = (Nodo a d xs)
                             | otherwise = (Nodo a b (la_agregar c d xs))


-- Dada una clave, elimina el dato asociado en la lista.
la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b
la_borrar _ (Vacia) = (Vacia)
la_borrar x (Nodo a b xs) | x==a = xs
                          | otherwise = (Nodo a b (la_borrar x xs))





