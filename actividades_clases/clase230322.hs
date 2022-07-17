factorialG :: Integer -> Integer
factorialG n | n == 0 = 1
             | otherwise  = n * factorialG(n-1)

fibonacciG :: Integer ->Integer
fibonacciG n | n == 0 = 0
             | n == 1 = 1
             |  otherwise  = fibonacciG(n-2) + fibonacciG(n-1)

-- factFib por guardas

factFib :: [Integer] -> [Integer]
factFib (c:r) | r == [] = [comprueba c]
              | otherwise = [comprueba c] ++ factFib r

-- Comprueba par o impar
comprueba :: Integer -> Integer
comprueba n | even n == True = factorialG n
            | otherwise = fibonacciG n

---------------------------------------------------------------------------
-- Definir la funcion factorial tal que factoriales n es la lista de los factoriales desde el factorial de 0 hasta el factorial n
-- Ejemplo: factoriales 5, regresa [1,1,2,6,24,120]

factoriales :: Integer -> [Integer]
factoriales n | n == 0 = [1]
              | otherwise = factoriales(n-1) ++ [factorialG n]
--            | n > 0 = [factorialG n | n <- [0..n]]

------------------------------------------------------------------
-- Funcion que regresa si el numero pertenece o no a la lista
-- Ejemplo: pertenece 3 [lista] false

pertenece :: Integer -> [Integer] -> Bool
pertenece n [] = False 
pertenece n (c:r) | n == c = True  
                  | otherwise = pertenece n r

------------------------------------------------------------------
--Funcion sucesion Fibonacci
sucFibonacci :: Integer -> [Integer]
sucFibonacci n | n == 0 = [0]
               | otherwise = sucFibonacci(n-1) ++ [fibonacciG n]

--Funcion que sume elementos de una lista(guardas)
sumaLista :: [Integer] -> Integer 
sumaLista [] = 0
sumaLista (c:r) | otherwise = c + sumaLista(r)

--Funcion que regrese la posicion de un elemento en una lista
--Ejemplo: posicion 4 [5,4,3,8,9] regrese 2
pos :: Integer -> [Integer] -> Integer
pos n [] = -1
pos n (c:r) | n == c = 1
                 | otherwise = 1 + pos n r

posicion :: Integer -> [Integer] -> Integer
posicion n l | pertenece n l == False = 0
             | otherwise = pos n l

--Funcion que calcule el tamaño de una lista
tamLista :: [Integer] -> Integer
tamLista [] = 0
tamLista (_:r) = 1 + tamLista r
