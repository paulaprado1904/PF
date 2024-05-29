module Main(main) where
main :: IO ()

area :: Float -> Float 
main = do putStrLn ("Insira o diâmetro do círculo:")
          diam <- getLine
          area = pi * ((diam/2)^2)
          putStrLn (show("A área do círculo é " ++ area))


--resolução:
area d = pi*r*r 
    where r = d/2
