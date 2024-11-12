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