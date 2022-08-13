{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.List
import qualified Data.Set as Set

indexed :: [a] -> [(a, Int)]
indexed xs = zip xs [0..]

manifold :: ((a -> b) -> b -> a -> b) -> [a] -> (a -> b) -> b
manifold f xs apply = foldl' (f apply) (apply (head xs)) (tail xs)
manifold𝟙 f xs = manifold f xs id

-- for simplicity, list is used instead of multiset

-- definition. (ℓ+1)^X := ∪ 2^Y where Y ∈ ℓ^X
ℓ_powerset :: (Ord a) => Int -> [a] -> [[a]]
ℓ_powerset ℓ u | ℓ >= 3 = foldr (++) [] succ
  where succ = [ ℓ_powerset 2 sublist | sublist <- pred ]
        pred = ℓ_powerset (ℓ - 1) u
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
varnothing = "_" -- ∅ for ASCII

class Prettify a where 
  prettify :: a -> String
  toTex :: a -> String

comma :: (t -> String) -> String -> t -> String
comma apply = \ x y -> x ++ ", " ++ apply y

instance Prettify [String] where
  prettify [] = varnothing
  prettify [[]] = '{' : varnothing ++ "}"
  prettify xs = '{' : manifold𝟙 comma xs ++ "}"
  toTex [] = "\\varnothing"
  toTex [[]] = "\\{\\varnothing\\}"
  toTex xs = "\\{" ++ manifold𝟙 comma xs ++ "\\}"

instance Prettify [[String]] where
  prettify xs = '{' : manifold comma xs prettify ++ "}"
  toTex xs = "\\{" ++ manifold comma xs toTex ++ "\\}"

-- toTex 


main = putStrLn $ prettify (ℓ_powerset 2 ["a", "b"])
-- [] :: [String]

