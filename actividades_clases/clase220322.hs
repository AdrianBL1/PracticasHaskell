--Formula General
formulaGeneral :: Float -> Float -> Float -> [Float]
formulaGeneral a b c = [(-b+sqrt(b*b-4.0*a*c)) / (2.0*a), 
                        (-b-sqrt(b*b-4.0*a*c)) / (2.0*a)]

formulaGeneral2 :: Float -> Float -> Float -> [Float]
formulaGeneral2 a b c = [(-b+d)/n,
                        (-b-d)/n]
                                where d = sqrt (b*b-4.0*a*c)
                                      n = 2.0*a

--------------------------------------------------------------

--Lista de cuadrados
cuadradosListaD :: Integer -> [Integer]
cuadradosListaD 1 = [1]
cuadradosListaD n = [n*n] ++ cuadradosListaD (n-1)


--GUARDAS

--Absoluto
absoluto :: Integer -> Integer
absoluto n | n < 0 = -n
           | n >= 0 = n

--Lista de cuadrados
cuadradosListaD_G :: Integer -> [Integer]
cuadradosListaD_G n | n <= 0 = []
                    | n > 0 = [n*n] ++ cuadradosListaD (n-1)

--Lista de cuadrados
cuadradosListaD_G2 :: Integer -> [Integer]
cuadradosListaD_G2 n | n <= 0 = []
                     | otherwise = [n*n] ++ cuadradosListaD (n-1)

-----------------------------------------------------------------------

-- Funcion Factorial
factorialG :: Integer -> Integer
factorialG n | n == 0 = 1
             | otherwise = n * factorialG (n-1)

-- Funcion Fibonacci
fibonacciG :: Integer -> Integer
fibonacciG n | n == 0 = 0
             | n == 1 = n
             | otherwise = fibonacciG(n-2) + fibonacciG(n-1)