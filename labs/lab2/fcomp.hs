--function composition of a list
compose :: [a -> a] -> a -> a
compose xs = foldr (.) id xs