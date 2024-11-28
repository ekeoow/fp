-- smallest integer that can be evenly devided by each of the numbers from 1 to n
smallestMultiple :: Integer -> Integer
smallestMultiple x = foldr (lcm) 1 [2..x]