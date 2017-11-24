

main xs = map id (reverse xs)

reverse = foldl (flip (:)) []

map f xs = case xs of
    [] -> []
    y:ys -> f y : map f ys

id x = x

flip f x y = f y x

foldl f z list = case list of
    []     -> z
    (x:xs) -> foldl f (f z x) xs

