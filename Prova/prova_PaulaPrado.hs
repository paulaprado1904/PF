-- Paula Prado Carvalho

distancia :: Float -> Float -> Float
distancia xb yb = sqrt ((xb - 0)^2 + (yb - 0)^2)

areaCirculo :: Float -> Float 
areaCirculo d = pi * (r^2)
     where r = d / 2

multa :: Float -> Float -> IO()
multa vel velMax
    | vel < velMax = putStrLn "O valor da multa é 0."
    | vel < ((120 / 100) * velMax) = putStrLn "O valor da multa é de 180R$."
    | otherwise = putStrLn "O valor da multa é de 600R$."

menor :: Int -> Int -> Int -> Int
menor a b c 
    | a <= b && a <= c = a
    | b <= a && b <= c = b
    | c <= a && c <= b = c

posicaoS :: Float -> Float -> Float -> Float
posicaoS s0 v0 t = let a = (-9.81)
                   in s0 + (v0*t) + ((1/2) * a * (t^2))