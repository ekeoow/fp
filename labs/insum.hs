intervalSums :: Int -> Int
intervalSums n = length [k | k <- [1..2 * n], 2 * n `mod` k == 0, isValid k] - 1
  where
    isValid k =
      let a = (2 * n `div` k - k + 1) `div` 2
      in a > 0  -- Ensure a is positive
