import Data.Bits

-- Infinite list of primes using the sieve of Eratosthenes
primes :: [Integer]
primes = 2 : sieve [3,5..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
    sieve [] = []

-- Check if a number is prime using the infinite primes list
isPrime :: Integer -> Bool
isPrime n
  | n < 2     = False
  | otherwise = all (\p -> n `mod` p /= 0) (takeWhile (\p -> p * p <= n) primes)

-- Modular exponentiation
expmod :: Integer -> Integer -> Integer -> Integer
expmod a 0 m = 1
expmod a n m
  | n .&. 1 == 0   = expmod (mod square m) (shiftR n 1) m
  | otherwise        = mod (a * (expmod a (n - 1) m)) m
  where square = a * a

-- Generate Poulet (Carmichael) numbers
pouletList :: [Integer]
pouletList = [n | n <- [9,11..], not (isPrime n), expmod 2 (n - 1) n == 1]

-- Get the nth Poulet number
poulet :: Int -> Int
poulet x = fromInteger (pouletList !! x)
