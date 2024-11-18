import Data.Bits
poulet :: Int -> Int
poulet x = fromInteger (pouletList !! x)


pouletList :: [Integer]
pouletList = [n | n <- [1..], not (isPrime n), expmod 2 (n-1) n == 1]

--modular exponentiation
expmod :: Integer -> Integer -> Integer -> Integer
expmod a 0 m = 1
expmod a n m
  | n .&. 1 == 0   = expmod (mod square m) (shiftR n 1) m
  | otherwise        = mod (a * (expmod a (n - 1) m)) m
  where square = a * a


--checker for prime numbers
isPrime :: Integer -> Bool
isPrime 2 = True
isPrime n = odd n && aux 3 n && n > 2

--helper function for checking prime numbers
aux :: Integer -> Integer -> Bool
aux m n = n < m * m || (mod n m /= 0 && aux (m + 2) n)

primes :: [Integer]
primes = 2:sieve [3,5..]
    where
        sieve (p:xs) = p : sieve [x | x <- xs, mod x p /= 0]