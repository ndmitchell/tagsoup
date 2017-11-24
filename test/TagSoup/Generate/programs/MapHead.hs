

main xs = map head xs

map f xs = case xs of
    [] -> []
    y:ys -> f y : map f ys

head x = case x of
    [] -> error "here"
    x:xs -> x

