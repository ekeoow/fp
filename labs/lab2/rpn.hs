import Data.Char

rpn2prefix :: String -> String
rpn2prefix input = parse (fixer $ lexer input) []

lexer :: String -> [String]
lexer [] = []
lexer (c:cs)
  | elem c "\n\t "    = lexer cs -- skip whitespace
  | elem c "+-*/%()"  = [c] : lexer cs
  | isDigit c         = (c : takeWhile isDigit cs) : lexer (dropWhile isDigit cs)

fixer :: [String] -> [String]
fixer = map fix
   where
      fix "/" = "div"
      fix x = x

isNumbers :: String -> Bool
isNumbers = all isDigit

parse :: [String] -> [String] -> String
parse [] [stack] = stack
parse (tok:tokens) stack
   | isNumbers tok = parse tokens (tok : stack)
   | otherwise = parse tokens (helper : stack')
      where
         (x : y : stack') = stack
         helper = "(" ++ "(" ++ tok ++ ")" ++ " " ++ y ++ " " ++ x ++ ")"