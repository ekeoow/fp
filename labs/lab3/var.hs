import Data.Char
import Data.List

variables :: String -> [(Int, String)]
variables xs = zip inf (sort $ nub $ lexer xs)
  where inf = [0..]


lexer :: String -> [String]
lexer [] = []
lexer (c:cs)
  | elem c "\n\t " = lexer cs
  | isAlpha c && isNotKeyword word = word : lexer rest
  | isAlpha c = lexer rest
  | otherwise = lexer cs
    where
      word = c : takeWhile isAlpha cs
      rest = dropWhile isAlpha cs


isNotKeyword :: String -> Bool
isNotKeyword input = not (word `elem` ["and", "entails", "or"])
  where word = map toLower input

-- main :: IO () 
-- main = do 
--   input <- readFile "input.in" 
--   let varList = variables input 
--   print varList
