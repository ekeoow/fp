nthSuperPrime :: Int -> Integer
nthSuperPrime x = sPrimes !! (x - 1)
    where
        sPrimes = [y | y <- [1..], isSuperPrime y]


--check if a number is prime
isPrime :: Integer -> Bool
isPrime n
    | n < 2 = False
    | n == 2 = True
    | n == 3 = True
    | otherwise = mod n 2 == 1 && mod n 3 /= 0  && aux 5 n 


--aux function to check if a number is prime
aux :: Integer -> Integer -> Bool
aux m n 
    | m * m > n = True
    | mod n m == 0 = False
    | mod n (m + 2) == 0 = False
    | otherwise = aux (m + 6) n

isSuperPrime :: Integer -> Bool
isSuperPrime x 
    | not (isPrime x) = False
    | x < 100 && x > 10 = and (map isPrime (listOfDigits x))
    | otherwise = and (map isPrime (combos(listOfDigits x)))

listOfDigits :: Integer -> [Integer]
listOfDigits 0 = []
listOfDigits x = listOfDigits (div x 10) ++ [mod x 10]

combos :: [Integer] -> [Integer]
combos xs = helper xs 0
    where
        helper xs i
            | i == length xs = []
            | otherwise = listToInt(take i xs ++ drop (i + 1) xs) : helper xs (i + 1)


listToInt :: [Integer] -> Integer
listToInt xs = aux xs 0
    where
        aux [] n = n
        aux (y:ys) n = aux ys (n * 10 + y)


