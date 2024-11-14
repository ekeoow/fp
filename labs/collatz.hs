import Data.List (genericLength)

--finding the collatz sequence of a number
collatz :: Integer -> [Integer]
collatz x
    | x == 1 = [1]
    | otherwise = x : collatz (collatzRule x)

--collatz rules
collatzRule :: Integer -> Integer
collatzRule 1 = 1
collatzRule n 
    | n `mod` 2 == 0 = n `div` 2
    | otherwise = 3 * n + 1

--list of collatz records
collatzRecords :: [Integer]
collatzRecords = collatzRecordsHelper 1 0

--function to find next record
collatzRecordsHelper :: Integer -> Integer -> [Integer]
collatzRecordsHelper n maxSteps
    | steps > maxSteps = n : collatzRecordsHelper (n + 1) steps
    | otherwise = collatzRecordsHelper (n + 1) maxSteps
    where
        steps = genericLength (collatz n) - 1

--nth collatz record
nthCollatzRecord :: Int -> Integer
nthCollatzRecord n = collatzRecords !! (n - 1)
