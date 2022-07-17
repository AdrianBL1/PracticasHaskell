--Suma Lista
sumaLista :: [Int] -> Int
sumaLista [] = 0
sumaLista (x:y) = x + sumaLista(y)

--Resta Lista
restaLista :: [Int] -> Int
restaLista [] = 0
restaLista (x:y) = x - restaLista(y)

--Multiplicar Lista
multLista :: [Int] -> Int
multLista [] = 1
multLista (x:y) = x * multLista(y)

--Obtener el Numero mayor de una lista
mayorLista :: [Int] -> Int
mayorLista [] = 0
mayorLista (x:y) =  if x > mayorLista y then x else mayorLista y

--Funcion que sume los cuadrados de una lista
cuadradosLista :: [Integer] -> Integer
cuadradosLista [] = 0
cuadradosLista (x:y) = (x*x) + cuadradosLista y

-------------------------------------------------------------------------

--Cuadrado 
cuadrado :: Integer -> Integer 
cuadrado n = n*n

--Suma elementos de una lista
sumaLista2 :: [Integer] -> Integer
sumaLista2 [] = 0
sumaLista2 (c:r) = c + sumaLista2 r

--Suma cuadrados de una lista
sumaCuadradosL :: [Integer] -> Integer
sumaCuadradosL lista = sumaLista2(map cuadrado lista)

-------------------------------------------------------------------------