

main f g xs = map f (map g xs)

map f xs = case xs of
    [] -> []
    y:ys -> f y : map f ys

