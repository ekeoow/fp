--power digits
powDigits :: Integer -> Integer -> Int -> Integer
powDigits n e d = expmod n e (10^d)

--modular exponentiation
expmod :: Integer -> Integer -> Integer -> Integer
expmod a 0 m = 1
expmod a n m
  | mod n 2 == 0   = expmod (mod square m) (div n 2) m
  | otherwise        = mod (a * (expmod a (n - 1) m)) m
  where square = a * a