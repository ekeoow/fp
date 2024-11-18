nthPalPrime :: Int -> Integer
nthPalPrime x 
    | x == 5 = 11
    | x < 5 = primePalindromes !! (x - 1)
    | otherwise = primePalindromes !! (x - 2)




isPrime :: Integer -> Bool
isPrime 2 = True
isPrime n = odd n && aux 3 n && n > 2

aux :: Integer -> Integer -> Bool
aux m n = n < m * m || (mod n m /= 0 && aux (m + 2) n)









palindromicPrimes :: [Integer]
palindromicPrimes = [d | d <- primes, isPalindrome d]

isPalindrome :: Integer -> Bool
isPalindrome x = x == reverse x
    where
        reverse y = aux 0 y
        aux w 0 = w
        aux w y = aux (w * 10 + (mod y 10)) (div y 10)

primes :: [Integer]
primes = 2:sieve [3,5..]
    where
        sieve (p:xs) = p : sieve [x | x <- xs, mod x p /= 0]


primePalindromes :: [Integer]
primePalindromes = filter isPrime oddDigitPalindromes

oddDigitPalindromes :: [Integer]
oddDigitPalindromes = [makeOddPalindrome n | n <- [1..]]

-- Function to create an odd-length palindrome from a number `n`
makeOddPalindrome :: Integer -> Integer
makeOddPalindrome n = n * (10 ^ numDigits (n `div` 10)) + reverseDigits (n `div` 10)

-- Helper function to count the number of digits in a number
numDigits :: Integer -> Integer
numDigits 0 = 0
numDigits x = 1 + numDigits (x `div` 10)

-- Helper function to reverse the digits of a number
reverseDigits :: Integer -> Integer
reverseDigits = go 0
  where
    go rev 0 = rev
    go rev x = go (rev * 10 + x `mod` 10) (x `div` 10)


