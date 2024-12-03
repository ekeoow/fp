decimalExpansion :: Int -> Int -> String
decimalExpansion num denom = show quotient ++ "." ++ decimalExpansionHelper (10 * remainder) denom ""
  where (quotient, remainder) = num `divMod` denom

decimalExpansionHelper :: Int -> Int -> String -> String
decimalExpansionHelper 0 _ acc = acc
decimalExpansionHelper num denom acc =
  let (quotient, remainder) = num `divMod` denom
  in decimalExpansionHelper (10 * remainder) denom (acc ++ show quotient)

