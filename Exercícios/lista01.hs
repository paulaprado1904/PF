inc:: Float -> Float
inc x = x + 1

square :: Float -> Float
square x = x*x

average :: Float -> Float -> Float
average a b = (a + b) / 2.0

areaRetangulo :: Int -> Int -> Int
areaRetangulo b h = b*h

areaQuadrado :: Int -> Int
areaQuadrado l = l^2

areaTriangulo :: Float -> Float -> Float
areaTriangulo b h = (b*h)/2

areaCirculo :: Float -> Float
areaCirculo r = pi*(r^2)

areaCoroa :: Float -> Float -> Float
areaCoroa r s = pi*(r^2) - pi*(s^2)

volumeCubo :: Int -> Int
volumeCubo l = l^3

volumePara :: Int -> Int -> Int -> Int
volumePara b h l = b*h*l

volumePiramide :: Float -> Float -> Float -> Float
volumePiramide b l h = (b*l*h)/3

volumeEsfera :: Float -> Float
volumeEsfera r = (4/3)*pi*(r^3)

hipotenusa :: Float -> Float -> Float
hipotenusa cat cate = sqrt(cat^2 + cate^2)

dist :: Float -> Float -> Float
dist x y = sqrt((x^2) + (y^2))

dist' :: Float -> Float -> Float -> Float -> Float
dist' xa ya xb yb = sqrt(((xb-xa)^2) + ((yb-ya)^2))

quadrado :: Int -> Int
quadrado x = x^2

cubo :: Int -> Int
cubo x = x^3

quarta :: Int -> Int
quarta x = (quadrado x)^2

segundosHoras :: Float -> Float
segundosHoras s = s/3600

segundosMinutos :: Float -> Float
segundosMinutos s = s/60

fahrenparacelsius :: Float -> Float
fahrenparacelsius x = (x-32)/(1.8)

celsiusparafahren :: Float -> Float
celsiusparafahren x = (1.8)*x + 32

kelvinparacelsius :: Float -> Float
kelvinparacelsius x = x - 273

fahrenparakelvin :: Float -> Float
fahrenparakelvin x = ((x-32)*5)/(9+273)

kmhparams :: Float -> Float
kmhparams x = x/(3.6)

ex28 :: Bool -> Bool -> Bool
ex28 p q = (p || q) && not(p && q)

ex29 :: Bool -> Bool -> Bool -> Bool
ex29 p q r = (p || q) && r

ex30 :: Bool -> Bool -> Bool -> Bool
ex30 p q r = (p && q) || not(p && r)

ex31 :: Bool -> Bool -> Bool -> Bool -> Bool
ex31 p q r s = p || (q && r) || not(s)

ex32 :: Bool -> Bool -> Bool -> Bool -> Bool
ex32 p q r s = not(p || q) && (r || s) && not(r)
