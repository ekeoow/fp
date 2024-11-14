fusc :: Integer -> Integer
fusc n = f n 0 1
    where
        f 0 a b = a
        f 1 a b = b
        f n a b
            | n == 2 * k = f k a (a + b)
            | n == 2 * k + 1 = f k b (a + b)
            where
                k = div n 2



aux :: Integer -> Integer
aux 0 = 0
aux 1 = 1
aux n
    | n == 2 * k = aux k
    | otherwise = aux k + aux (k + 1)
    where
        k = div n 2 

aux1 :: Integer -> Integer
aux1 n = iter n
  where
    iter m
      | m == 0 = 0
      | m == 1 = 1
      | m `mod` 2 == 0 = iter k
      | otherwise = iter k + iter (k + 1)
      where
        k = div m 2







f n
    | n < 2 = 1
    | n == 2*k = 2*f k + f (k-1)
    | otherwise = 3*f k
    where k = n `div` 2

f1 n = h n 1 0
    where
    h 0 a b = a+b
    h 1 a b = a+3*b
    h n a b
        | n == 2*k = h (k - 1) a (2*a + 3*b)
        | n == 2*k + 1 = h k (3*a + b) (2*b)
        where k = n `div` 2