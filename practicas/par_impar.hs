-- Funcion que determine par o impar sin utlizar even
par :: Int -> Bool
par n = if mod n 2==0 then True else False

impar :: Int -> Bool
impar n = if mod n 2==0 then False else True