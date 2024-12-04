import Data.Char
import Text.Printf

{- parser for the grammar:
    S -> F S'
    S' -> * F S' 
    S' -> / F S'
    S' -> % F S'
    S' -> <empty string>
    F -> <digits>
    F -> -F
    F -> (S)
-}

data TokOperation = Add TokOperation TokOperation
                 | Sub TokOperation TokOperation
                 | Mul TokOperation TokOperation
                 | Div TokOperation TokOperation
                 | Mod TokOperation TokOperation
                 | Neg TokOperation
                 | Num Integer
                 deriving (Show, Eq)

eval :: TokOperation -> Integer
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 `div` eval e2
eval (Mod e1 e2) = eval e1 `mod` eval e2
eval (Neg e)     = -(eval e)
eval (Num n)     = n

type Token  = String
type Tokens = [String]

lexer :: String -> Tokens
lexer [] = []
lexer str@(c:cs)
  | elem c "\n\t "  = lexer cs -- skip whitespace
  | elem c "*/%-()"  = [c] : lexer cs
  | isDigit c       = takeWhile isDigit str : lexer (dropWhile isDigit str)
  | otherwise       = error $ printf "Illegal character '%c' found." c

parseS :: (TokOperation, Tokens) -> (TokOperation, Tokens)
parseS = parseS' . parseF

parseS' :: (TokOperation, Tokens) -> (TokOperation, Tokens)
parseS' (acc, "*":toks) = let (f, toks') = parseF (Num 0, toks)
                          in parseS' (Mul acc f, toks')
parseS' (acc, "/":toks) = let (f, toks') = parseF (Num 0, toks)
                          in parseS' (Div acc f, toks')
parseS' (acc, "%":toks) = let (f, toks') = parseF (Num 0, toks)
                          in parseS' (Mod acc f, toks')
parseS' (acc, toks)     = (acc, toks)

parseF :: (TokOperation, Tokens) -> (TokOperation, Tokens)
parseF (acc, []) = (acc, [])
parseF (acc, tok:toks)
    | tok == "("         = parseF' (acc, toks)
    | tok == "-"         = let (f, toks') = parseF (Num 0, toks)
                          in parseS' (Neg f, toks')
    | isDigit (head tok) =  (Num (read tok), toks)
    | otherwise          = error $ printf "Error, unexpected '%s'." tok

parseF' :: (TokOperation, Tokens) -> (TokOperation, Tokens)
parseF' (acc, []) = (acc, [])
parseF' (acc, toks)
  | head toks == ")" = (acc, tail toks)
  | otherwise = let (s, toks') = parseS (acc, toks) in
    if head toks' == ")" then (s, tail toks')
    else error "Error: Uneven amount of parentheses."

parser :: String -> (Integer, Tokens)
parser str = let (s,toks') = parseS (Num 0, lexer str) 
             in (eval s, toks')
