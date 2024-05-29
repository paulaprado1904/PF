numRaizes a b c 
    | (delta > 0) = 2
    | (delta == 0) = 1
    | otherwise = 0
    where delta = b*b - 4*a*c
