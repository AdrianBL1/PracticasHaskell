-- Arbol binario

data ArbolB a = VacioB | NodoB (ArbolB a) a (ArbolB a) deriving Show
a2 :: ArbolB Integer
a2 = NodoB aI 10 aD
    where
        aI = NodoB aII 15 aID
        aD = NodoB aDI 18 aDD
        aII = hojaB 24
        aID = hojaB 27
        aDI = VacioB
        aDD = hojaB 24

hojaB :: a -> ArbolB a
hojaB x = NodoB VacioB x VacioB

raizB :: ArbolB a -> a
raizB VacioB = error "raiz de arbol vacio"
raizB (NodoB _ x _) = x

tamanoB :: ArbolB a -> Integer
tamanoB VacioB = 0
tamanoB (NodoB i r d) = 1 + tamanoB  i + tamanoB d

profundidadB :: ArbolB a -> Integer
profundidadB VacioB = 0
profundidadB (NodoB i r d) = 1 + max (profundidadB i) (profundidadB d)


-- Recorrido

enOrdenB :: ArbolB a -> [a]
enOrdenB VacioB = []
enOrdenB (NodoB i r d) = enOrdenB i ++ (r: enOrdenB d)

preOrdenB :: ArbolB a -> [a]
preOrdenB VacioB = []
preOrdenB (NodoB i r d) = (r : preOrdenB i) ++ preOrdenB d

postOrdenB :: ArbolB a -> [a]
postOrdenB VacioB = []
postOrdenB (NodoB i r d) = postOrdenB i ++ postOrdenB d ++ [r]