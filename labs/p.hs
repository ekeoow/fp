import Data.Bits

--check if a number is prime
isPrime :: Integer -> Bool
isPrime 2 = True
isPrime n = mod n 2 == 1 && mod n 3 /= 0  && aux 5 n 

--aux function to check if a number is prime
aux :: Integer -> Integer -> Bool
aux m n 
    | m * m > n = True
    | mod n m == 0 = False
    | mod n (m + 2) == 0 = False
    | otherwise = aux (m + 6) n

--infinite list of poulet numbers
poulets :: [Integer]
poulets = [n | n <- [1..], not(isPrime n), expmod 2 (n-1) n == 1]

--specific poulet number
poulet :: Int -> Int
poulet x = fromInteger (poulets !! x)

--modular exponentiation
expmod :: Integer -> Integer -> Integer -> Integer
expmod a 0 m = 1
expmod a n m
  | n .&. 1 == 0   = expmod (mod square m) (shiftR n 1) m
  | otherwise        = mod (a * (expmod a (n - 1) m)) m
  where square = a * a
