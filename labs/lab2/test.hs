import System.IO.Unsafe
import System.Exit
import Data.Char

-- Error handling functions
abortParser :: String -> (Integer, [String])
abortParser errMessage = unsafePerformIO (die errMessage)

abortLexer :: String -> [String]
abortLexer errMessage = unsafePerformIO (die errMessage)

-- Lexer
lexer :: String -> [String]
lexer [] = []
lexer (c:cs)
  | elem c "\n\t "    = lexer cs -- skip whitespace
  | elem c "+-*/%()"  = [c] : lexer cs
  | isDigit c         = (c : takeWhile isDigit cs) : lexer (dropWhile isDigit cs)
  | otherwise         = abortLexer $ "Lexical error: invalid character '" ++ [c] ++ "' in input"

-- Parsing functions
parseS :: [String] -> (Integer, [String])
parseS = parseE

-- Parse E -> T E'
parseE :: [String] -> (Integer, [String])
parseE tokens = parseE' acc rest
  where (acc, rest) = parseT tokens

-- Parse E' -> + T E' | - T E' | <empty>
parseE' :: Integer -> [String] -> (Integer, [String])
parseE' acc ("+":tokens) =
  let (val, rest) = parseT tokens
   in parseE' (acc + val) rest
parseE' acc ("-":tokens) =
  let (val, rest) = parseT tokens
   in parseE' (acc - val) rest
parseE' acc tokens = (acc, tokens)

-- Parse T -> F T'
parseT :: [String] -> (Integer, [String])
parseT tokens = parseT' acc rest
  where (acc, rest) = parseF tokens

-- Parse T' -> * F T' | / F T' | % F T' | <empty>
parseT' :: Integer -> [String] -> (Integer, [String])
parseT' acc ("*":tokens) =
  let (val, rest) = parseF tokens
   in parseT' (acc * val) rest
parseT' acc ("/":tokens) =
  let (val, rest) = parseF tokens
   in parseT' (acc `div` val) rest
parseT' acc ("%":tokens) =
  let (val, rest) = parseF tokens
   in parseT' (acc `mod` val) rest
parseT' acc tokens = (acc, tokens)

-- Parse F -> (E) | <integer>
parseF :: [String] -> (Integer, [String])
parseF ("(":tokens) =
  let (val, rest) = parseE tokens
   in case rest of
        (")":rest') -> (val, rest')
        _ -> abortParser "Parse error: missing closing parenthesis"
parseF (tok:tokens)
  | all isDigit tok = (read tok :: Integer, tokens)
  | otherwise       = abortParser $ "Syntax Error: unexpected '" ++ tok ++ "'"
parseF [] = abortParser "Parse error: unexpected end of input"

-- Top-level parser function
parser :: String -> (Integer, [String])
parser str = parseS (lexer str)

-- Evaluate expression
parseEvalExpr :: String -> Integer
parseEvalExpr str =
  case parser str of
    (val, []) -> val
    (_, rest) -> error $ "Parse error: unprocessed input '" ++ unwords rest ++ "'"

