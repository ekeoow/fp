type Clause = ([Int], [Int]) -- (Positive literals, Negative literals)
type Clauses = [Clause]

-- Function to resolve two clauses
resolve :: Clause -> Clause -> [Clause]
resolve (pos1, neg1) (pos2, neg2) = 
    [ (filter (/= v) (union pos1 pos2), filter (/= v) (union neg1 neg2)) 
    | v <- pos1, v `elem` neg2 ] ++
    [ (filter (/= v) (union pos1 pos2), filter (/= v) (union neg1 neg2)) 
    | v <- pos2, v `elem` neg1 ]
    where
        union xs ys = xs ++ filter (`notElem` xs) ys

-- Function to check if the empty clause is present
isEmptyClause :: Clause -> Bool
isEmptyClause ([], []) = True
isEmptyClause _        = False



-- Function to check if two clauses are resolvable
resolvable :: Clause -> Clause -> Bool
resolvable x y = helper x y || helper y x
    where 
        helper a b = any (`elem` (snd b)) (fst a)

-- Function to apply resolution rule to a set of clauses
resolutionProof :: Clauses -> Bool
resolutionProof clauses = resolution [] clauses
    where
        resolution processed [] = False
        resolution processed (c:cs)
            | isEmptyClause c = True
            | otherwise = let newClauses = [r | p <- processed, resolvable p c, r <- resolve p c, not (r `elem` processed)]
                              nextClauses = [r | r <- newClauses, not (r `elem` cs)]
                          in resolution (c : processed) (cs ++ nextClauses)
