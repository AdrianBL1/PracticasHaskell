--Funcion que calcula el factorial de los numeros pares
-- Por ejemplo, funparesfact 8, regresa [2,24,720,40320]

factorialG :: Integer -> Integer
factorialG n | n == 0 = 1
              | otherwise  = n * factorialG(n-1)

par :: Integer -> Bool
par n = if mod n 2==0 then True else False

funparesfact :: Integer  -> [Integer]
funparesfact n | n < 2 = []
               | par n == True = funparesfact (n-1) ++ [factorialG n]
               | otherwise = funparesfact (n-1)

--Funcion que multiplica los elementos de una lista (guardas)

multLista :: [Int] -> Int
multLista [] = 0
multLista (x:y) | otherwise = product (x:y)

--------------------------------------------------------------------

multiplica :: [Integer] -> Integer
multiplica (c:r) | r == [] = c
                 | otherwise = c * multiplica r

--Funcion que regrese el valor de una posicion de la lista
--Ejemplo: valorposicion 4 [5,4,2,8,9]

valorposicion :: Integer -> [Integer] -> Integer
valorposicion n [] = 0
valorposicion n (c:r) | n == 1 = c
                      | otherwise = valorposicion (n-1) r

--Funcion que cuente los numeros imapres de una lista
--Ejemplo: cuentaimpar [5,4,6,7,8,2,3,5] devuelve 4

impar :: Int -> Bool
impar n | n == n = if mod n 2==0 then False else True

cuentaimpar :: [Int] -> Int
cuentaimpar n | n == n = (length(filter(impar) n))