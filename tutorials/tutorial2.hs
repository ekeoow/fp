import Data.Char

elemNum2 x xs = length ((filter) (==x) xs)

-- elemNum :: Integer -> [Integer] -> Integer
-- elemNum _ [] = 0
-- elemNum x (y:ys)
--     | x == y = 1 + (elemNum x ys)
--     | otherwise = elemNum x ys



-- iSort :: [Integer] -> [Integer]
-- iSort [] = []
-- iSort (x:xs) = ins x (iSort xs)

-- ins :: Integer -> [Integer] -> [Integer]
-- ins x [] = [x]
-- ins x (y:ys)
--     | x >= y = x:(y:ys)
--     | otherwise = y:(ins x ys)

-- sublist :: Eq a => [a] -> [a] -> Bool
-- sublist [] _ = True
-- sublist + [] = False
-- sublist (x:xs) (y:ys)
--     | x == y = sublist xs ys
--     | otherwise = sublist (x:xs) ys


-- subsequence :: Eq a => [a] -> [a] -> Bool
-- subsequence [] _ = True
-- subsequence _ [] = False
-- subsequence (x:xs) (y:ys)
--     | x == y = zip xs ys == zip xs xs

palindrome xs = filtered == reverse filtered
    where filtered = [toLower x | x <- xs, isAlpha x]