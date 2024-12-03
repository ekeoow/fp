import Data.List 

fibcat :: [Integer]
fibcat = nub $ merge fib cat
-- code for this function taken from slides
merge :: [Integer] -> [Integer] -> [Integer]
merge (x:xs) (y:ys)
    | x==y = x:merge xs ys
    | x < y = x:merge xs (y:ys)
    | x > y = y:merge (x:xs) ys
-- code for this function taken from slides
fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)

cat :: [Integer]
cat = 1 : [g x | x <- [1..]]        
    where
        g n = div (x n) (y n)
        x n = factorial (2 * n)
        y n = factorial n * factorial (n + 1)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = product [1..n]