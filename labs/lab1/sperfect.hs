import Data.List 

divisors :: Int -> [Int]
divisors n = [x | x <- [1..j], mod n x == 0]
    where j = (div n 2) + 1

isPerfect :: Int -> Bool
isPerfect x = elem x (map (sum) (subsequences (divisors x)))

snumbers :: [Int]
snumbers = [x | x <- [6..], isPerfect x]

isMultipleOfAny :: [Int] -> Int -> Bool
isMultipleOfAny lst n = any (\x -> n `mod` x == 0) lst

generatePerfect :: [Int]
generatePerfect = helper [6] [6..]
  where
    helper semiPerfects (x:xs)
      | isPerfect x = x : helper (x : semiPerfects) xs
      | isMultipleOfAny semiPerfects x = x : helper semiPerfects xs
      | otherwise = helper semiPerfects xs



multiples :: Int -> [Int]
multiples x = [x * y | y <- [1..10]]

nthSemiPerfectNumber:: Int -> Int
nthSemiPerfectNumber x = generatePerfect !! (x - 1)
