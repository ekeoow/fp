numPals :: [Integer]
numPals = 9 : aux 18 0 1
   where
      aux x y z
         | mod y 2 /= 0 = x : aux (helper x z) (y + 1) (z + 1)
         | otherwise = x : aux (helper2 x z) (y + 1) z

helper :: Integer -> Integer -> Integer
helper x y = x + 9 * 10 ^ y

helper2 :: Integer -> Integer -> Integer
helper2 x y  = fst k * (10 ^ (y + 1)) + snd k
   where k = divMod x (10^y)

