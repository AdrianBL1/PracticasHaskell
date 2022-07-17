perimetroRectangulo :: Float -> Float -> Float
perimetroRectangulo ladoA ladoB = (ladoA + ladoB) * 2

perimetroRombo :: Float -> Float -> Float
perimetroRombo a b = 2 * (a + b)

perimetroPoligono :: Float -> Float -> Float
perimetroPoligono longitud nLados = longitud * nLados


areaTriangulo :: Float -> Float -> Float
areaTriangulo b h = (b * h) / 2

areaCirculo :: Float -> Float
areaCirculo r = pi * (r * r)

areaTrapecio :: Float -> Float -> Float -> Float
areaTrapecio altura baseMayor baseMenor = altura * (baseMayor * baseMenor) / 2