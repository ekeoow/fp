-- import Data.Char (toLower, isSpace)

-- -- Function to split the input into premises and goals
-- splitInput :: String -> ([String], [String])
-- splitInput input = (parseSection premises, parseSection goals)
--   where
--     -- Split input into premises and goals based on "ENTAILS"
--     (premisesPart, goalsPart) = breakEntails input
    
--     -- Remove brackets and parse each section
--     premises = stripBrackets premisesPart
--     goals = stripBrackets goalsPart
    
--     -- Helper function to parse a section into individual clauses
--     parseSection :: String -> [String]
--     parseSection = map trim . splitBy ';'
    
-- -- Break input into premises and goals based on "ENTAILS"
-- breakEntails :: String -> (String, String)
-- breakEntails input = (unwords (takeWhile (not . isEntails) parts), unwords (drop 1 $ dropWhile (not . isEntails) parts))
--   where
--     parts = words input
--     isEntails word = map toLower word == "entails"

-- -- Remove enclosing square brackets
-- stripBrackets :: String -> String
-- stripBrackets str = filter (\c -> c /= '[' && c /= ']') str

-- -- Trim leading and trailing whitespace
-- trim :: String -> String
-- trim = f . f
--   where f = reverse . dropWhile isSpace

-- -- Split a string by a given delimiter
-- splitBy :: Char -> String -> [String]
-- splitBy delim = foldr op [[]]
--   where
--     op c acc
--       | c == delim = [] : acc
--       | otherwise = (c : head acc) : tail acc

-- main :: IO () 
-- main = do 
--   input <- readFile "input.in" 
--   let varList = splitInput input 
--   print varList

import Data.Char (toLower, isSpace)

-- Function to split the input into premises and goals
splitInput :: String -> ([String], [String])
splitInput input = (parseSection premises, parseSection goals)
  where
    -- Split input into premises and goals based on "ENTAILS"
    (premisesPart, goalsPart) = breakEntails input
    
    -- Remove brackets and parse each section
    premises = stripBrackets premisesPart
    goals = stripBrackets goalsPart
    
    -- Helper function to parse a section into individual clauses
    parseSection :: String -> [String]
    parseSection "" = []  -- If the string is empty, return an empty list
    parseSection section = map trim (splitBy ';' section)
    
-- Break input into premises and goals based on "ENTAILS"
breakEntails :: String -> (String, String)
breakEntails input = (unwords (takeWhile (not . isEntails) parts), unwords (drop 1 $ dropWhile (not . isEntails) parts))
  where
    parts = words input
    isEntails word = map toLower word == "entails"

-- Remove enclosing square brackets
stripBrackets :: String -> String
stripBrackets str = filter (\c -> c /= '[' && c /= ']') str

-- Trim leading and trailing whitespace
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- Split a string by a given delimiter
splitBy :: Char -> String -> [String]
splitBy delim = foldr op [[]]
  where
    op c acc
      | c == delim = [] : acc
      | otherwise = (c : head acc) : tail acc

main :: IO () 
main = do 
  input <- readFile "input.in" 
  let varList = splitInput input 
  print varList
