lista :: [Int] -> [Int] -- Tomar valores
lista [] = [] -- Generar un arreglo
lista (x:xs) | x < calcularPromedio (x:xs) = [0] ++ lista (xs) -- Manda a ejecutar Promedio, compara valores con el promedio y gernera lista
                |otherwise = [1] ++ lista (xs) -- Si el valor de la lista es menor o igual al promedio añade 0, caso contrario añade 1

calcularPromedio :: [Int] -> Int -- Toma datos para el promedio
calcularPromedio (x:xs) = sum(x:xs) `div` length(x:xs) -- Calcula promedio