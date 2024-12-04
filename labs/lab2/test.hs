import Data.List
import Text.Printf
lcm' :: Integer -> Integer -> Integer
lcm' a b = abs (a * b) `div` (gcd a b)
