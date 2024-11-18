--nth pal prime
nthPalPrime :: Int -> Integer
nthPalPrime x 
    | x == 5 = 11
    | x < 5 = primePalindromes !! (x - 1)
    | otherwise = primePalindromes !! (x - 2)

--infinite list of palindromic primes
primePalindromes :: [Integer]
primePalindromes = [p | n <- [1..], let p = palindromeCreator n, isPrime p]

--create all odd palindromes
palindromeCreator :: Integer -> Integer
palindromeCreator n = n * (10 ^ digits (div n 10)) + numberReverse (div n 10)

--reverse of a number
numberReverse :: Integer -> Integer
numberReverse x
    | x >= 0 && x < 10 = x
    | otherwise = (mod x 10) * (10 ^ (digits (div x 10))) + numberReverse (div x 10)

--number of digits in a number
digits :: Integer -> Integer
digits 0 = 0
digits x = 1 + digits (div x 10)

--checker for prime numbers
isPrime :: Integer -> Bool
isPrime 2 = True
isPrime n = odd n && aux 3 n && n > 2

--helper function for checking prime numbers
aux :: Integer -> Integer -> Bool
aux m n = n < m * m || (mod n m /= 0 && aux (m + 2) n)