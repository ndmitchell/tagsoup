

main x = not `dol` even x


dol x y = x y

not x = case x of
    True -> False
    False -> True


even x = mod x 2 == 0
