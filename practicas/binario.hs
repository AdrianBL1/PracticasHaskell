binario :: Int -> [Int]
binario 0 -> [0]
binario n -> binario (div n 2) ++ [(mod n 2)]