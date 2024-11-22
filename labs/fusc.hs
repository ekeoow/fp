fusc1 :: Integer -> Integer
fusc1 0 = 0
fusc1 1 = 1
fusc1 n
    | n == 2 * k = fusc k
    | otherwise = fusc (k) + fusc (k + 1)
    where k = div n 2


fusc :: Integer -> Integer
fusc n = h n 1 0
    where
        h 0 a b = b
        h 1 a b = a + b
        h n a b 
            | n == 2 * k = h k (a + b) b
            | n == 2 * k + 1 = h k a (a + b)
            where k = div n 2