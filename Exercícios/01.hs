--a)
xor a b = (a || b) && not (a && b)

--b)
xor' a b = a /= b 

--c)
xor'' a b = if (a /= b) then True else False

--d)
xor''' a b
    | a /= b = True
    | a == b = False

