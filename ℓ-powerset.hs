{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.List
import qualified Data.Set as Set

delta :: Bool -> a -> a -> a
delta True  x _ = x
delta False _ y = y

indexed :: [a] -> [(a, Int)]
indexed xs = zip xs [0..]

manifold :: ((a -> b) -> b -> a -> b) -> [a] -> (a -> b) -> b
-- assume xs != [], head or tail [] => exception
manifold f xs trans = foldl' (f trans) (trans (head xs)) (tail xs)
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
-- theorem. 1^X = {X}
ℓ_powerset ℓ u | ℓ == 1 = [u]
-- proof. (1) 2^X = ∪ 2^Y where Y ∈ 1^X 
--        (2) |1^X| = 1^|X| = 1, so 2^X = 2^Y, X = Y
--        (3) in summary, 1^X = {X}


-- by the definition, this can also be understood as 1^∅
𝜔1 = Set.singleton Set.empty -- {∅}


-- definition. order γ for X := |{x ∈ X : x = γ }| i.e. number of γ
order :: (Eq a) => [a] -> a -> Int
order xs x | xs == [] = 0
order xs x = foldl' (\ s t -> s + delta (t == x) 1 0) 0 xs

-- theorem. (a) order  ∅  for (ℓ+1)^X = ℓ^2
--          (b) order {x} for (ℓ+1)^X, ∀ x ∈ X = ℓ
--          (c) order  X  for (ℓ+1)^X = 1

-- proof. ...
 
-- theorem. if S = ℓ^X, then ℓ = 1 + order {x} for S, ∀ x ∈ X
--       ⟺ ℓ = 1 + sum 1 for Y ∈ ℓ^X, |Y| = 1


-- prettify 
varnothing = "_" -- ∅ for ASCII

class Prettify a where 
  prettify :: a -> String
  toTex :: a -> String

comma :: (t -> String) -> String -> t -> String
comma trans = \ x y -> x ++ ", " ++ trans y

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

main = 
-- putStrLn (prettify set)
-- putStrLn (toTex set)
     print (foldl' (\ s t -> s + delta (length t == 0) 1 0) 0 set)
  >> print (foldl' (\ s t -> s + delta (length t == 1) 1 0) 0 set)
  >> print (foldl' (\ s t -> s + delta (length t == 2) 1 0) 0 set)
  >> print (foldl' (\ s t -> s + delta (length t == 3) 1 0) 0 set)
  >> print (foldl' (\ s t -> s + delta (length t == 4) 1 0) 0 set)
  where set = ℓ_powerset 2 ["a", "b", "c", "d"]
-- [] :: [String]

