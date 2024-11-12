-- Ex 3.5

nAnd :: Bool -> Bool -> Bool
nAnd True True = False
nAnd x y = True

nAnd2 :: Bool -> Bool-> Bool
nAnd2 x y = not (x && y)