
#ifndef BASIS_HS
#define BASIS_HS

import Data.Char
import Data.List

type ℤ = Integer
type 𝕾 = String

𝛅 :: Bool -> a -> a -> a
𝛅 True  x _ = x
𝛅 False _ y = y

-- Data.List.transpose
𝛕 :: [[𝖆]] -> [[𝖆]] 
𝛕 ([]:_) = []
𝛕 xs = map head xs : 𝛕 (map tail xs)

indexed :: [a] -> [(a, Int)]
indexed xs = zip xs [0..]

#endif
