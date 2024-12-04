numPals :: [Integer]
numPals = 9:pally 


pally :: [Integer]
pally = p 0 18 1 
 where
  p x y c 
   | even x = y : p (x+1) (zeroAdder y c) c 
   | odd x = y : p (x+1) (nineAdder y c) (c+1) 


zeroAdder :: Integer -> Integer -> Integer 
zeroAdder x y = (x `div` 10^y)*10^(y+1) + (x `mod` 10^y) 

nineAdder :: Integer -> Integer -> Integer
nineAdder x y = x + 9*(10^(y))