import Data.Char

rpn2prefix :: String -> String
rpn2prefix xs = head (rpn2prefix' (lexxy xs))

rpn2prefix' :: [String] -> [String]
rpn2prefix' [] = []
rpn2prefix' (tok:toks)
    | isDigit (head tok) = tok : rpn2prefix' toks
    | tok == "+" || tok == "-" || tok == "*" || tok == "/" = 
        let (y:x:rest) = rpn2prefix' toks
        in ("((" ++ convertOp tok ++ ") " ++ x ++ " " ++ y ++ ")") : rest
    | otherwise = error "Syntax Error: invalid character in input"

convertOp :: String -> String
convertOp "+" = "+"
convertOp "-" = "-"
convertOp "*" = "*"
convertOp "/" = "div"

lexxy :: String -> [String]
lexxy xs = lexer xs []

lexer :: String -> [String] -> [String]
lexer [] acc = reverse acc
lexer (c:cs) acc
    | elem c "\n\t " = lexer cs acc
    | elem c "*/+-" = lexer cs ([c] : acc)
    | isDigit c = let (digits, rest) = span isDigit (c:cs)
                  in lexer rest (digits : acc)
    | otherwise = error "Syntax Error: invalid character in input"
