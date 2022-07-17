-- Primera Funcion
funcion1 :: Float -> Float
funcion1 1 = sin 1
funcion1 n = log n

-- Funcion que comprueba si n > m
funcion2 :: Integer -> Integer -> Bool 
funcion2 n m = n > m

-- Mayor de dos numeros
mayor1 :: Integer -> Integer -> Integer
mayor1 n m = max n m

-- Mayor de tres numeros
mayor2 :: Integer -> Integer -> Integer -> Integer
mayor2 n m o = mayor1 n (mayor1 m o)

-- Media de tres numeros
media :: Float -> Float -> Float -> Float
media n m o = (product [n,m,o])/3

-- Funcion que determine un impar
impar :: Int -> Bool
impar n = not (even n)

-- Funcion que determine par o impar sin utlizar even
par :: Int -> Bool
par n = if mod n 2==0 then True else False