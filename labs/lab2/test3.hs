rpn2prefix :: String -> String
rpn2prefix input = process (words input) []

process :: [String] -> [String] -> String
process [] [result] = result  -- At the end, the stack should have one final result
process (token:tokens) stack
    | isNumber token = process tokens (token : stack)  -- Push number to stack
    | isOperator token =
        let (x:y:rest) = stack
            operator = mapOperator token  -- Map "/" to "div"
            combined = "(" ++ "(" ++ operator ++ ")" ++ " " ++ y ++ " " ++ x ++ ")"  -- Parenthesize operator and operands
        in process tokens (combined : rest)

-- Map "/" to "div" and leave other operators unchanged
mapOperator :: String -> String
mapOperator "/" = "div"
mapOperator op  = op

isNumber :: String -> Bool
isNumber = all (`elem` ['0'..'9'])

isOperator :: String -> Bool
isOperator token = token `elem` ["+", "-", "*", "/"]
