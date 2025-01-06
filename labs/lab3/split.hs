import Data.Char 

splitInput :: String -> ([String], [String])
splitInput xs = (first, second)
  where
    (one, two) = splitAtEntails xs
    first = map lexer (split one)
    second = lexer two

lexer :: String -> String
lexer [] = []
lexer (c:cs)
  | elem c "\n\t"    = lexer cs -- skip whitespace
  | elem c "+*/%[]"  = lexer cs
  | otherwise         = c : lexer cs


testing :: (String, String) -> String
testing (x, y) = lexer y

isEntails :: String -> Bool
isEntails input = map toLower input == "entails"

splitAtEntails :: String -> (String, String)
splitAtEntails s = (unlines firstPart, unlines rest)
  where
    (firstPart, _:rest) = splitAt entailsIndex (lines s)
    entailsIndex = length (takeWhile (not . isEntails) (lines s))

split :: String -> [String]
split [] = []
split xs = rule : split rest
  where 
    rule = takeWhile (/= ';') xs
    rest = drop 1 (dropWhile (/= ';') xs)





main :: IO ()
main = do
  input <- readFile "input.in" 
  let (firstPart, secondPart) = splitAtEntails input
  putStrLn $ "First part: " ++ firstPart
  putStrLn $ "Second part: " ++ secondPart
  print $ testing (firstPart, secondPart)



import Data.Char

-- Function to split a string by semicolon
split :: String -> [String]
split [] = []
split xs = rule : split rest
  where
    rule = takeWhile (/= ';') xs
    rest = drop 1 (dropWhile (/= ';') xs)

-- Lexer function
lexer :: String -> String
lexer [] = []
lexer (c:cs)
  | elem c "\n\t"    = lexer cs -- skip whitespace
  | elem c "+*/%[]"  = lexer cs
  | otherwise        = c : lexer cs

-- Testing function
testing :: (String, String) -> String
testing (x, y) = lexer y

-- Check if the input is "Entails"
isEntails :: String -> Bool
isEntails input = map toLower input == "entails"

-- Split at "Entails" function
splitAtEntails :: String -> (String, String)
splitAtEntails s = (firstPart, restPart)
  where
    (beforeEntails, afterEntails) = span (not . isEntails) (words s)
    firstPart = unwords beforeEntails
    restPart = unwords (drop 1 afterEntails)

-- Split input function
splitInput :: String -> ([String], String)
splitInput xs = (first, second)
  where
    (one, two) = splitAtEntails xs
    first = map lexer (split one)
    second = lexer two

-- Main function
main :: IO ()
main = do
  input <- readFile "input.in"
  let (processedFirstPart, processedSecondPart) = splitInput input
  putStrLn $ "Processed first part: " ++ show processedFirstPart
  putStrLn $ "Processed second part: " ++ processedSecondPart
