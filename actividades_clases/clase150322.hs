-- Primera Lista
lista :: [Integer] -> Integer
lista [] = 0
lista (c:r) = c

-- Busca lista vacia
lista2 :: [Integer] -> Integer
lista2 [] = 0
lista2 (x:y) = lista2(y)

-- Cuenta el tamaño de una lista
tamL :: [Integer] -> Integer
tamL [] = 0
tamL (x:y) = 1 + lista2(y)

-- Factorial
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

-- Factorial Lista
factLista :: [Integer] -> [Integer]
factLista [] = []
factLista (x:y) = factorial x : factLista y

factLista2 :: [Integer] -> [Integer]
factLista2 [] = []
factLista2 (x:y) = [factorial x] ++ factLista2 y

factLista3 :: [Integer] -> [Integer]
factLista3 [] = []
factLista3 (x:y) = concat[[factorial x],factLista3 y]
