-- Check if a number is a Poulet number
isPoulet :: Integer -> Bool
isPoulet n = (2^(n-1) `mod` n) == 1 && not (isPrime n)

-- Check if a number is prime
isPrime :: Integer -> Bool
isPrime k | k < 2     = False
          | k == 2    = True
          | even k    = False
          | otherwise = null [ x | x <- [3,5..floor (sqrt (fromIntegral k))], k `mod` x == 0]

-- Find the nth Poulet number
poulet :: Int -> Integer
poulet n = pouletNumbers !! (n-1)
  where
    pouletNumbers = filter isPoulet [2..]

