
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Char
import Data.List

delta :: Bool -> a -> a -> a
delta True  x _ = x
delta False _ y = y

indexed :: [a] -> [(a, Int)]
indexed xs = zip xs [0..]

main = putStrLn $ table
  [ [" ", "A", "B"],  
    ["X", "AX   ゴ", "BX かか"], 
    ["Y", "AY ああ", "BY   ん"] ]

-- occup `|  |` := 4
fenceOccup = 4

class OccupComputable a where
  occup :: a -> Int
  occupWithFence :: a -> Int
  occupWithFence x = fenceOccup + occup x

instance OccupComputable Char where
  occup x = delta (isAscii x) 1 2

-- `(+) . occup` as `\ x y -> y + occup x`
instance OccupComputable String where
  occup xs = foldr (\ x y -> y + occup x) 0 xs

-- disc
occupBound :: [String] -> Int
occupBound xs = foldr (max . occup) 0 xs
occupBoundWithFence xs = fenceOccup + occupBound xs

lineHeight xs = foldr (\ x y -> y + 𝛍𝚮 x) 1 xs
  where 𝛍𝚮 x = delta (x == '\n') 1 0

--- --- --- --- --- --- --- --- --- --- --- --- ---

space n = replicate n ' '
spaceX left right s = space left ++ s ++ space right

data HorizonForm = HorBoth
  | HorLeft 
  | HorRight 
  deriving (Eq, Show)

line :: HorizonForm -> Int -> String
line HorLeft  n = '+' : replicate (n - 2) '-'
line HorRight n =       replicate (n - 2) '-' ++ ['+']
line HorBoth  n = '+' : replicate (n - 2) '-' ++ ['+']

-- `(. line HorRight) . (++)` as `\ x y -> x ++ line HorRight y`
lines𝖱 :: [Int] -> String
lines𝖱 xs = foldl 
  ((. line HorRight) . (++)) 
  (line HorBoth (head xs)) 
  (tail xs) ++ "\n"

fence :: HorizonForm -> String -> Int -> Int -> String
fence HorLeft  s left right = '|' : ' ' : spaceX left right s
fence HorRight s left right =       ' ' : spaceX left right s ++ " |"
fence HorBoth  s left right = '|' : ' ' : spaceX left right s ++ " |"

fences𝖱 xs = foldl 
  (\ x y -> x ++ fence HorRight y 0 0)
  (fence HorBoth (head xs) 0 0) 
  (tail xs) ++ "\n"

fences𝖱𝛘 xs ns = foldl
  (\ x y -> x ++ fence HorRight (fst y) 0 (ns !! snd y))
  (fence HorBoth (head xs) 0 (head ns)) 
  (tail (indexed xs)) ++ "\n"

halfOffset :: Int -> Int -> Int
halfOffset available occup = div (available - occup) 2

box s = line HorBoth (bound) ++ "\n" ++ 
  fence HorBoth s 0 0 ++ "\n" ++
  line HorBoth (bound)
  where bound = occupWithFence s

boxLocate bound s = line HorBoth (bound) ++ "\n" ++ 
  fence HorBoth s left right ++ "\n" ++
  line HorBoth (bound)
  where occupt = occup s
        availa = bound - fenceOccup
        left = halfOffset availa occupt
        right = availa - left - occupt
        
arrow HorLeft n  =  '<' : replicate (n - 1) '-'
arrow HorRight n =        replicate (n - 1) '-' ++ ['>']
arrow HorBoth n  =  '<' : replicate (n - 2) '-' ++ ['>']

--- --- --- --- --- --- --- --- --- --- --- --- ---

rowEntire𝖱 xs = lines𝖱 ns ++ fences𝖱 xs ++ lines𝖱 ns 
  where ns = map occupWithFence xs

-- 𝛘: absolute length list
rowCentre𝖱𝛘 xs 𝛘 = fences𝖱𝛘 xs h𝛘
  where h𝛘 = map d𝛘 (indexed 𝛘)
        d𝛘 = \ x -> (fst x) - occupWithFence (xs !! (snd x))

rowEntire𝖱𝛘 xs 𝛘 = lines𝖱 𝛘 ++ rowCentre𝖱𝛘 xs 𝛘 ++ lines𝖱 𝛘

table :: [[String]] -> String
table xs = foldl 
  (\ x y -> x ++ rowCentre𝖱𝛘 y bounds)
  (rowEntire𝖱𝛘 (head xs) bounds)
  (tail xs) ++ lines𝖱 bounds
  where bounds = map occupBoundWithFence (transpose xs)
