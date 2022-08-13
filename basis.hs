
#ifndef BASIS_HS
#define BASIS_HS

import Data.Char
import Data.List

type ℤ = Integer
type 𝕾 = String

delta :: Bool -> a -> a -> a
delta True  x _ = x
delta False _ y = y

-- Data.List.transpose
𝛕 :: [[𝖆]] -> [[𝖆]] 
𝛕 ([]:_) = []
𝛕 xs = map head xs : 𝛕 (map tail xs)

indexed :: [a] -> [(a, Int)]
indexed xs = zip xs [0..]

manifold :: ((a -> b) -> b -> a -> b) -> [a] -> (a -> b) -> b
manifold f xs apply = foldl' (f apply) (apply (head xs)) (tail xs)
manifold𝟙 f xs = manifold f xs id

(∈) :: Eq a => a -> [a] -> Bool
x ∈ xs = x `elem` xs

(∋) :: Eq a => [a] -> a -> Bool
xs ∋ x = x `elem` xs

(⊂) :: Eq a => [a] -> [a] -> Bool
x ⊂ y = x `isSubsequenceOf` y

#endif
