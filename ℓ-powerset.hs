{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

import qualified Data.Set as Set

#include "basis.hs"

-- for simplicity, list is used instead of multiset

-- definition. (ℓ+1)^X := ∪ 2^Y where Y ∈ ℓ^X
ℓ_powerset :: Int -> [a] -> [[a]]
ℓ_powerset ℓ u | ℓ >= 3 = foldr (++) [] succ
  where succ = [ ℓ_powerset 2 sublist | sublist <- pred ]
        pred = ℓ_powerset (ℓ-1) u
ℓ_powerset ℓ u | ℓ == 2 = [[ fst pair | pair <- xs ] 
    | subset <- redundance, let xs = Set.toList subset ]
  where redundance = Set.toList (Set.powerSet uniqueized)
        uniqueized = Set.fromList (indexed u)
ℓ_powerset ℓ u | ℓ == 1 = [u]
-- proof. (1) 2^X = ∪ 2^Y where Y ∈ 1^X 
--        (2) |1^X| = 1^|X| = 1, so 2^X = 2^Y, X = Y
--        (3) in summary, 1^X = {X}

-- by the definition, this can also be understood as 1^∅
𝜔1 = Set.singleton Set.empty -- {∅}

-- prettify 

-- toTex 
varnothing = "∅"

main = print $ ℓ_powerset 3 ["a", "b"]

