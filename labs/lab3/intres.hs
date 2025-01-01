import Debug
import Data.List

type Clause = ([Int], [Int])
type Clauses = [Clause]

--resolutionProof :: Clauses -> Bool


emptyClause :: Clause -> Bool
emptyClause x
    | x == ([], []) = True
    | otherwise = False

resolvable :: Clause -> Clause -> Bool
resolvable x y = helper x y || helper y x
    where helper a b = any (`elem` (snd b)) (fst a)

resolve :: Clause -> Clause -> [Clause]
resolve (pos1, neg1) (pos2, neg2) = 
    [ (filter (/= v) (union pos1 pos2), filter (/= v) (union neg1 neg2)) 
    | v <- pos1, v `elem` neg2 ] ++
    [ (filter (/= v) (union pos1 pos2), filter (/= v) (union neg1 neg2)) 
    | v <- pos2, v `elem` neg1 ]
    where
        union xs ys = xs ++ filter (`notElem` xs) ys


-- Resolution proof function
resolutionProof :: Clauses -> Bool
resolutionProof clauses = resolutionLoop clauses []

-- Recursive resolution loop
resolutionLoop :: Clauses -> Clauses -> Bool
resolutionLoop known newClauses
    | any emptyClause newClauses = True
    | null newClauses = False
    | otherwise = resolutionLoop (known ++ newClauses) (concatMap (uncurry resolve) resolvablePairs)
    where
        pairs = [(x, y) | x <- known ++ newClauses, y <- known ++ newClauses, x /= y]
        resolvablePairs = filter (uncurry resolvable) pairs