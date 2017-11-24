

main = reverse

reverse = foldl (flip (:)) []

flip f x y = f y x

foldl f z list = case list of
    []     -> z
    (x:xs) -> foldl f (f z x) xs
