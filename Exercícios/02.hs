import GHC.Platform.ArchOS (ArmABI(HARD))
fpar a
    | (mod a 2 == 0) = True
    | (mod a 2 /= 0) = False

-- ou)
fpar' a = if (mod a 2 == 0) then True else False


