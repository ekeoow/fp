intervalSums :: Int -> Int
intervalSums 1 = 0
intervalSums n 
  | mod n 2 == 1 = aux 1 0 1 1
  | otherwise = aux 0 0 1 1
  where
    aux i sum x y
      | x > div (n - 2) 2 = i 
      | sum < n = aux i (sum + y) x (y + 1)
      | sum > n = aux i (sum - x) (x + 1) y
      | otherwise = aux (i + 1) (sum - x + y) (x + 1) (y + 1)