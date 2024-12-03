import Data.Char
import Text.ParserCombinators.Parsec.Prim (token)

rpn2prefix :: String -> String
rpn2prefix xs = (rpn2prefix1 xs) ++ (rpn2prefix2 xs)

rpn2prefix1 :: String -> String
rpn2prefix1 xs = r2p "" (lexxy xs) (lexxySignCount (lexxy xs))
 where
  r2p str (tok:token) c
   | c==0 = str ++ tok
   | (head tok) == '/' = r2p (str ++ "((div) ") token (c-1)
   | otherwise = r2p (str ++ "((" ++ tok ++ ") ") token (c-1)

rpn2prefix2 :: String -> String
rpn2prefix2 xs = r2p "" (drop (fromIntegral(lexxySignCount (lexxy xs)+1)) (lexxy xs)) (lexxyNumCount (lexxy xs)-2)
 where
  r2p str (tok:token) c
   | c==0 = str ++ " " ++ tok ++ ")"
   | otherwise = r2p (str ++ " " ++ tok ++ ")") token (c-1)

lexxyNumCount :: [String] -> Integer 
lexxyNumCount xs = sum[1 | d <- xs, isDigit (head d)]

lexxySignCount :: [String] -> Integer 
lexxySignCount xs = sum[1 | d <- xs, not (isDigit (head d))]

lexxy :: String -> [String]
lexxy xs = reverse (lexer1 xs) ++ lexer2 xs

lexer1 :: String -> [String]
lexer1 [] = []
lexer1 (c:cs)
 | elem c "\n\t " = lexer1 cs
 | elem c "*/+-" = [c]:(lexer1 cs)
 | isDigit c = lexer1(dropWhile isDigit cs)
 | otherwise = error "Syntax Error: invalid character in input"

lexer2 :: String -> [String]
lexer2 [] = []
lexer2 (c:cs)
 | elem c "\n\t " = lexer2 cs
 | elem c "*/+-" = lexer2 cs
 | isDigit c = (c:takeWhile isDigit cs): lexer2(dropWhile isDigit cs)
 | otherwise = error "Syntax Error: invalid character in input"
