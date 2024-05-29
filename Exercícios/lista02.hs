type Tupla3 = (Int, Int, Int)
ex1 :: Tupla3 -> Int
ex1 (a, b, c)
  | a == b && a == c = 3
  | a == b || b == c || a == c = 2
  | otherwise = 0

type Tupla2 = (Int, Int)
ex2 :: Tupla2 -> Int
ex2 (a, b) = if (a<b) then a else b

ex3 :: Tupla3 -> Int
ex3 (a, b, c) = if (a<b && a<c)
                   then a
                   else if (b<a && b<c)
                    then b
                     else c

ex4 :: Tupla3 -> Tupla2
ex4 (a, b, c)
  | a > b && a > c && b < c = (a, b)
  | a > b && a > c && b > c = (a, c)
  | b > a && b > c && a < c = (b, a)
  | b > a && b > c && c < a = (b, c)
  | c > a && c > b && a < b = (c, a)
  | c > a && c > b && b < a = (c, b)

ex5 :: Int -> Int -> Int -> Int
ex5 a b c
  | b^2 > 4*a*c = 2
  | b^2 == 4*a*c = 1
  | b^2 < 4*a*c = 0

ex6 :: Tupla3 -> Tupla3 -> IO() 
ex6 (a, m, d) (a2, m2, d2)
  | (a, m, d) < (a2, m2, d2) = putStr"Primeira data ocorreu antes da segunda \n"
  | (a, m, d) > (a2, m2, d2) = putStr"Segunda data ocorreu antes da primeira \n"
  | otherwise = putStr"As duas datas são a mesma \n"

-- ex7
ordena2if :: Int -> Int -> (Int, Int)
ordena2if x y = if (x>y) then (y, x) else (x, y)

ordena2guarda :: Int -> Int -> (Int, Int)
ordena2guarda x y 
  | x>y = (y, x)
  | otherwise = (x, y)

-- ex8
par :: Int -> Bool
par x = if (mod x 2 == 0) then True else False

-- ex9
impar :: Int -> Bool
impar x = if (mod x 2 /= 0) then True else False

-- ex10 --------------------------------------
-- FUNÇÕES LÓGICAS ---------------------------

not1log :: Bool -> Bool
not1log p = not(p)

and1log :: Bool -> Bool -> Bool
and1log p q = p && q

or1log :: Bool -> Bool -> Bool
or1log p q = p || q

nandlog :: Bool -> Bool -> Bool
nandlog p q = not(p && q)

norlog :: Bool -> Bool -> Bool
norlog p q = not(p || q)

xorlog :: Bool -> Bool -> Bool
xorlog p q = (p || q) && not(p && q)

xnorlog :: Bool -> Bool -> Bool
xnorlog p q = not((p || q) && not(p && q))

-- ex10 --------------------------------------
-- IF ----------------------------------------

not1if :: Bool -> Bool
not1if p = if p==True then False else True

and1if :: Bool -> Bool -> Bool
and1if p q = if (p || q == False) then False else True

or1if :: Bool -> Bool -> Bool
or1if p q = if (p || q == True) then True else False

nandif :: Bool -> Bool -> Bool
nandif p q = if (p || q == False) then True else False

norif :: Bool -> Bool -> Bool
norif p q = if (p || q == True) then False else True

xorif :: Bool -> Bool -> Bool
xorif p q = if (p /= q) then True else False

xnorif :: Bool -> Bool -> Bool
xnorif p q = if (p /= q) then False else True

-- ex10 --------------------------------------
-- GUARDA ------------------------------------

not1guar :: Bool -> Bool
not1guar p
 | p == True = False
 | p == False = True

and1guar :: Bool -> Bool -> Bool
and1guar p q
 | p == True && q == True = True
 | otherwise = False 

or1guar :: Bool -> Bool -> Bool
or1guar p q
 | p == True || q == True = True
 | otherwise = False

nandguar :: Bool -> Bool -> Bool
nandguar p q
 | p == True && q == True = False
 | otherwise  = True 

norguar :: Bool -> Bool -> Bool
norguar p q
 | p == True || q == True = False
 | otherwise = True

xorguar :: Bool -> Bool -> Bool
xorguar p q
 | p /= q = True
 | otherwise = False

xnorguar :: Bool -> Bool -> Bool
xnorguar p q
 | p /= q = False
 | otherwise = True

ex11 :: Int -> String
ex11 m 
 | m==1 = "Janeiro"
 | m==2 = "Fevereiro"
 | m==3 = "Marco"
 | m==4 = "Abril"
 | m==5 = "Maio"
 | m==6 = "Junho"
 | m==7 = "Julho"
 | m==8 = "Agosto"
 | m==9 = "Setembro"
 | m==10 = "Outubro"
 | m==11 = "Novembro"
 | m==12 = "Dezembro"
 | otherwise = "Erro!"

ex12 :: Int -> Int -> Int -> Int -> Int -> Int -> IO() 
ex12 a m d a2 m2 d2
  | (a, m, d) < (a2, m2, d2) = putStr"Primeira data ocorreu antes da segunda \n"
  | (a, m, d) > (a2, m2, d2) = putStr"Segunda data ocorreu antes da primeira \n"
  | otherwise = putStr"As duas datas são a mesma \n"

ex13 :: Int -> Int -> Int -> String
ex13 a b c
  | possivel = if (a==b && b==c)
                then "Equilatero"
                 else if (a/=b && b/=c) then "Escaleno"
                  else "Isosceles"
  | otherwise = "Nao eh triangulo"
  where
    possivel = a>0 && b>0 && c>0 && a+b>c && a+c>b && b+c>a

ex14 :: Float -> Float -> IO()
ex14 vel vmax
  | vel <= vmax = putStr"Sem multa \n"
  | vel > vmax && vel <= 1.2*vmax = putStr"Multa de 180 reais \n"
  | vel > 1.2*vmax = putStr"Multa de 600 reais \n"

ex15 :: Float -> Float
ex15 salario
  | salario <= 1317.07 = salario*0.08
  | salario >= 1317.08 && salario <= 2195.12 = salario*0.09
  | salario >= 2195.13 && salario <= 4390.24 = salario*0.11
  | salario > 4390.24 = 483.93

ex16 :: Int -> Int -> IO()
ex16 idade tempo
  | idade >= 65 || tempo > 30 || (idade >= 60 && tempo >= 25) = putStr"Pode se aposentar \n"
  | otherwise = putStr"Nao pode se aposentar \n"

ex18 cx cy px py r
  | sqrt((px - cx)^2 + (py - cy)^2) == r = putStr"O ponto pertence a circunferencia \n"
  | sqrt((px - cx)^2 + (py - cy)^2) < r = putStr"O ponto esta dentro da circunferencia \n"
  | otherwise = putStr"O ponto esta fora da circunferencia \n"  