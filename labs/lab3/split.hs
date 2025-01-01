import Data.Char 



split :: String -> [String]
split [] = []
split input = initial : split (tail rest)
    where
        (initial, rest) = span (/= ';') input

lexer :: String -> [String]
lexer [] = []
lexer (c:cs)
  | elem c "\n\t "    = lexer cs -- skip whitespace
  | elem c "+-*/%()[]"  = lexer cs
  | otherwise         = [c] : lexer cs

