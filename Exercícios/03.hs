fimpar a
    | (mod a 2 == 0) = False
    | (mod a 2 /= 0) = True

-- ou)
fimpar' a = if (mod a 2 == 0) then False else True