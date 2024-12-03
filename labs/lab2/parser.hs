import Data.Char

type Token  = String
type Tokens = [String]

lexer :: String -> Tokens
lexer [] = []
lexer str@(c:cs)
  | elem c "\n\t "  = lexer cs -- skip whitespace
  | elem c "*/%()-+"  = [c]:(lexer cs)
  | isDigit c    = takeWhile isDigit str : lexer(dropWhile isDigit str)
  | otherwise    = abort $ printf "illegal character '%c' found." c

parseS :: (Integer,Tokens) -> (Integer,Tokens)
parseS = parseS'.parseF

parseS' :: (Integer,Tokens) -> (Integer,Tokens)
parseS' (acc,"*":toks) = let (val, rest) = parseF (0, toks) in parseS' (acc * val, rest)
parseS' (acc,"/":toks) = let (val, rest) = parseF (0, toks) in parseS' (acc `div` val, rest)
parseS' (acc,"%":toks) = let (val, rest) = parseF (0, toks) in parseS' (acc `mod` val, rest)
parseS' (acc,"+":toks) = let (val, rest) = parseF (0, toks) in parseS' (acc + val, rest)
parseS' (acc,"-":toks) = let (val, rest) = parseF (0, toks) in parseS' (acc - val, rest)
parseS' (acc,toks)     = (acc, toks)

parseF :: (Integer,Tokens) -> (Integer,Tokens)
parseF (_,[])      =  abort "error: unexpected end of input."
parseF (_,tok:toks)
  | isDigit (head tok) =  ((read tok :: Integer), toks)
  | otherwise          =  abort $ printf "Error, unexpected '%s'." tok

parser :: String -> (Integer,Tokens)
parser str = parseS ((read "" :: Integer),lexer str)

