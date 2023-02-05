
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

import Data.List
import qualified Data.Set as Set

delta :: Bool -> a -> a -> a
delta True  x _ = x
delta False _ y = y

indexed :: [a] -> [(a, Int)]
indexed xs = zip xs [0..]

-- assume xs != [], head or tail [] => exception
manifold :: ((a -> b) -> b -> a -> b) -> [a] -> (a -> b) -> b
manifold f xs trans = foldl' (f trans) (trans (head xs)) (tail xs)
manifold𝟙 :: ((b -> b) -> b -> b -> b) -> [b] -> b
manifold𝟙 f xs = manifold f xs id

-- for simplicity, list is used instead of multiset

-- definition. (ℓ+1)^X := ∪ 2^Y where Y ∈ ℓ^X
ℓ_powerset :: (Ord a) => Int -> [a] -> [[a]]
ℓ_powerset ℓ u | ℓ >= 3 = concat succ
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

ℓ_powerset _ _ = []

-- by the definition, this can also be understood as 1^∅
𝜔1 = Set.singleton Set.empty -- {∅}


-- definition. order γ for X := |{ x ∈ X : x ≅ γ }| i.e. number of γ
order :: (Eq a) => [a] -> a -> Int
order xs x | null xs = 0
order xs x = foldl' (\ s t -> s + delta (t == x) 1 0) 0 xs

-- theorem. order γ for ℓ^X where γ ∈ 2^X = (|X| choose |γ|) (ℓ - 1)^|X|
-- proof. ...

-- corollary. (a) order ∅ for ℓ^X = (ℓ - 1)^|X|
--            (b) ∑ order γ for ℓ^X where γ ∈ 2^X = ℓ^|X|

-- theorem. if S = ℓ^X, then ℓ = 1 + order {x} for S, ∀ x ∈ X
--       ⟺ ℓ = 1 + sum 1 for Y ∈ ℓ^X, |Y| = 1

-- prettify 
varnothing :: [Char]
-- varnothing = "_" -- ∅ for ASCII
varnothing = "∅"

class Prettify a where
  prettify :: a -> String
  toTex :: a -> String

comma :: (t -> String) -> String -> t -> String
comma trans x y = x ++ ", " ++ trans y

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


land :: (t -> String) -> String -> t -> String
land trans x y = x ++ " & " ++ trans y

drawTexTable :: [[String]] -> String
drawTexTable xs = "\\def\\arraystretch{1.5}" ++
  "\\begin{array}{" ++ layout ++ "}" ++ "\\hline\n" ++
  foldl' handle [] xs ++
  "\\end{array}"
  where rowNumber = length (head xs)
        spacec = replicate rowNumber 'c'
        layout = foldl' (\ s t -> s ++ [t] ++ "|") "|" spacec
        handle = \ s t -> s ++ "  " ++ manifold𝟙 land t ++ " \\\\ \\hline\n"

-- escape (github, markdown, etc.)
drawTexTableEscape :: [[String]] -> String
drawTexTableEscape xs = "\\begin{array}{" ++ layout ++ "}" ++ 
  "\\hline\n" ++ foldl' handle [] xs ++ "\\end{array}"
  where rowNumber = length (head xs)
        spacec = replicate rowNumber 'c'
        layout = foldl' (\ s t -> s ++ [t] ++ "|") "|" spacec
        handle = \ s t -> s ++ "  " ++ manifold𝟙 land t ++ " \\\\\\ \\hline\n"

indexs :: Foldable t => t a -> [Int]
indexs xs = [0 .. length xs - 1]

border :: [a] -> [a] -> [[a]] -> [[a]]
border hs ls xs = hs : [ls !! i : xs !! i | i <- indexs ls]

table :: Int -> Int -> (Int -> Int -> a) -> [[a]]
table m n f = [[f i j | i <- [1 .. m]]
                      | j <- [1 .. n]]

ordℓXn :: [[a]] -> Int -> Int
ordℓXn ℓX n = foldl' (\ s t -> s + delta (length t == n) 1 0) 0 ℓX

-- for katex syntax
texℓXOrdTable :: (Ord a) => [a] -> Int -> Int -> String
texℓXOrdTable g n card = drawTexTable (border hs ls xs)
  where ts = "\\mathcal{O}_{\\ell\\le" ++ show n ++ "}(\\Z/" ++ show (length g) ++"\\Z)"
        hs = ts : ["\\ell=" ++ show ℓ | ℓ <- [1 .. n]]
        ls = ["|\\gamma|=" ++ show γ | γ <- [0 .. card]]
        xs = table n (card + 1) (\ x y -> show (ordℓXn (ℓ_powerset x g) (y - 1)))

-- escape (github, markdown, etc.)
texℓXOrdTableEscape :: (Ord a) => [a] -> Int -> Int -> String
texℓXOrdTableEscape g n card = drawTexTableEscape (border hs ls xs)
  where ts = "\\mathcal{O}_{\\ell\\le" ++ show n ++ "}(\\mathbb{Z}/" ++ show (length g) ++"\\mathbb{Z})"
        hs = ts : ["\\ell=" ++ show ℓ | ℓ <- [1 .. n]]
        ls = ["|\\gamma|=" ++ show γ | γ <- [0 .. card]]
        xs = table n (card + 1) (\ x y -> show (ordℓXn (ℓ_powerset x g) (y - 1)))


texℓXOrdTable' :: Int -> Int -> String
texℓXOrdTable' n card = texℓXOrdTable [0 .. card -  1] n card

texℓXOrdTableEscape' :: Int -> Int -> String
texℓXOrdTableEscape' n card = texℓXOrdTableEscape [0 .. card -  1] n card

texInlineMathEnv, texBlockMathEnv :: String -> String
texInlineMathEnv s = '$' : s ++ "$"
texBlockMathEnv s = "$$" ++ s ++ "$$"

info :: [String] -> Int -> IO ()
info u n = putStrLn (str ++ ": " ++ len)
  where str = prettify set
        len = show (length set)
        set = ℓ_powerset n u
           
main :: IO ()
main = -- putStrLn (foldl' (\ s t -> s ++ f t) "" [2 .. 4])
  -- where f n = texBlockMathEnv (texℓXOrdTableEscape' 4 n) ++ "\n\n"
     info ["a", "b"] 2 >> putStrLn []
  >> info ["a", "b"] 3 >> putStrLn []
  >> info ["a", "b"] 4 >> putStrLn []
  >> info ["a", "b"] 5 >> putStrLn []
  
-- print (ordℓXn (ℓ_powerset 3 [0, 1]) 1) 
-- putStrLn (prettify set)
-- putStrLn (toTex set)

  --    print (foldl' (\ s t -> s + delta (length t == 0) 1 0) 0 set)
  -- >> print (foldl' (\ s t -> s + delta (length t == 1) 1 0) 0 set)
  -- >> print (foldl' (\ s t -> s + delta (length t == 2) 1 0) 0 set)
  -- >> print (foldl' (\ s t -> s + delta (length t == 3) 1 0) 0 set)
  -- >> print (foldl' (\ s t -> s + delta (length t == 4) 1 0) 0 set)

  -- where set = ℓ_powerset 2 ["a", "b", "c", "d"]
-- [] :: [String]

