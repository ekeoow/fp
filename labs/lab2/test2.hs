type Clause = [String]

resolutionProof :: [Clause] -> Clause -> Bool
resolutionProof premises goal = deriveResolution (premises ++ [negateClause goal]) []

-- Negate a clause (negates each literal in the clause)
negateClause :: Clause -> Clause
negateClause = map negateLiteral

-- Negate a single literal
negateLiteral :: String -> String
negateLiteral ('~':x) = x  -- Remove negation
negateLiteral x       = '~' : x  -- Add negation

-- Check if two literals are complementary
isComplementary :: String -> String -> Bool
isComplementary x y = negateLiteral x == y

-- Resolve two clauses to produce a new clause (if possible)
resolveClauses :: Clause -> Clause -> [Clause]
resolveClauses c1 c2 = 
  [ (removeLiteral x c1 ++ removeLiteral y c2)
  | x <- c1, y <- c2, isComplementary x y
  ]
  where
    removeLiteral lit = filter (/= lit)

-- Derive resolution iteratively
deriveResolution :: [Clause] -> [Clause] -> Bool
deriveResolution [] _ = False  -- No more clauses to resolve
deriveResolution clauses visited
  | [] `elem` newClauses = True  -- Found the empty clause
  | null newClauses = False  -- No new clauses generated
  | otherwise = deriveResolution (clauses ++ newClauses) (visited ++ clauses)
  where
    newClauses = [ resolvent 
                 | (c1, c2) <- allPairs clauses
                 , resolvent <- resolveClauses c1 c2
                 , resolvent `notElem` (clauses ++ visited)
                 ]

-- Generate all pairs of clauses
allPairs :: [a] -> [(a, a)]
allPairs xs = [(x, y) | (x:ys) <- tails xs, y <- ys]
