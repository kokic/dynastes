
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Ratio

delta :: Bool -> a -> a -> a
delta True  x _ = x
delta False _ y = y

data OneVariableMonomial = OneVariableMonomial {
  variable :: String, 
  coefficient :: Rational,
  power :: Rational
} deriving (Eq)

type Monomial = [OneVariableMonomial]
type Polynomial = [Monomial]

occup :: Integer -> Int
occup x = length (show x)

class Prettify a where 
  prettify :: a -> String
  toTex :: a -> String

bundle left right n = delta (occup n > 1) (left ++ s ++ right) s where s = show n
paren = bundle "(" ")"
brace = bundle "{" "}"

instance Prettify Rational where
  prettify x | x == 1 = [] | q == 1 = paren p
    | otherwise = show p ++ '/' : show q
    where (p, q) = (numerator x, denominator x)
  toTex x | x == 1 = [] | q == 1 = brace p
    | otherwise = "\\frac" ++ brace p ++ brace q
    where (p, q) = (numerator x, denominator x)

instance Prettify OneVariableMonomial where
  prettify x = (prettify (coefficient x)) ++ (variable x) ++ '^' : (prettify (power x))
  toTex x = (toTex (coefficient x)) ++ (variable x) ++ '^' : (toTex (power x))


-- data Polynomial = OneVariableMonomialCase OneVariableMonomial 

instance Num OneVariableMonomial where
  (+) x y = OneVariableMonomial "x" (coefficient x + coefficient y) (power x)



x = OneVariableMonomial "x" (7 / (-3)) 5
y = OneVariableMonomial "x" 2 5

main = putStrLn $ prettify z ++ "\n" ++ toTex z where z = x + y



