import Data.Char

-- Main function to convert RPN to prefix
rpn2prefix :: String -> String
rpn2prefix input = parse (lexer input) []

-- Lexer splits the input string into tokens (numbers and operators)
lexer :: String -> [String]
lexer [] = []
lexer (c:cs)
  | elem c "\n\t "    = lexer cs  -- Skip whitespace
  | elem c "+-*/%()"  = [c] : lexer cs  -- Operators are individual tokens
  | isDigit c         = (c : takeWhile isDigit cs) : lexer (dropWhile isDigit cs)  -- Numbers are grouped together

-- Parse the RPN expression to produce the prefix notation
parse :: [String] -> [String] -> String
parse [] [stack] = stack  
parse (tok:tokens) stack
    | isNumbers tok = parse tokens (tok : stack)
    | otherwise = parse tokens (asdf : stack')
        where
            (x:y:stack') = stack
            asdf = "(" ++ "(" ++ tok ++ ")" ++ " " ++ y ++ " " ++ x ++ ")"

-- Map "/" to "div" and leave other operators unchanged
mapOperator :: String -> String
mapOperator "/" = "div"
mapOperator op  = op

-- Check if a string represents a number


isNumbers :: String -> Bool
isNumbers = all isDigit