-- Arbol #1 Ejemplo

data Arbol a = Vacio | Nodo a [Arbol a] deriving Show
a1 :: Arbol Integer
a1 = Nodo 10 [a11, a12, a13]
    where 
        a11 = Nodo 22 [hoja 15, hoja 12]
        a12 = hoja 35
        a13 = Nodo 52 [hoja 33]

profundidad :: Arbol a -> Integer
profundidad Vacio = 0
profundidad (Nodo _ []) = 1
profundidad (Nodo _ xs) = (+1) . maximum . map profundidad $ xs

hoja :: a -> Arbol a
hoja x = Nodo x []

raiz :: Arbol a -> a
raiz Vacio = error "raiz de arbol vacio"
raiz (Nodo x _) = x

tamano :: Arbol a -> Integer
tamano Vacio = 0
tamano (Nodo _ xs) = (+1) . sum . map tamano $ xs