
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Ratio

delta :: Bool -> a -> a -> a
delta True  x _ = x
delta False _ y = y

bundle left right n = delta (occup n > 1) (left ++ s ++ right) s where s = show n
paren = bundle "(" ")"
brace = bundle "{" "}"

occup :: Integer -> Int
occup x = length (show x)

class Prettify a where 
  prettify :: a -> String
  toTex :: a -> String

instance Prettify Rational where
  prettify x | x == 1 = [] | q == 1 = paren p
    | otherwise = show p ++ '/' : show q
    where (p, q) = (numerator x, denominator x)
  toTex x | x == 1 = [] | q == 1 = brace p
    | otherwise = "\\frac" ++ brace p ++ brace q
    where (p, q) = (numerator x, denominator x)



data OneVariableMonomial = OneVariableMonomial {
  variable :: String, 
  coefficient :: Rational,
  power :: Rational
} deriving (Eq)

oneMonomial variable coefficient power = OneVariableMonomial variable coefficient power
oneMonomialX = oneMonomial "x"
oneMonomialC coefficient = oneMonomial "" coefficient 0

type Monomial = [OneVariableMonomial]
type Polynomial = [Monomial]

prettifyPowerF :: String -> Rational -> String
prettifyPowerF _ 0 = []
prettifyPowerF variable 1 = variable
prettifyPowerF variable power = variable ++ '^' : (prettify power)

toTexPowerF :: String -> Rational -> String
toTexPowerF _ 0 = []
toTexPowerF variable 1 = variable
toTexPowerF variable power = variable ++ '^' : (toTex power)

instance Prettify OneVariableMonomial where
  prettify x = (prettify (coefficient x)) ++ prettifyPowerF (variable x) (power x)
  toTex x = (toTex (coefficient x)) ++ toTexPowerF (variable x) (power x)

instance Num OneVariableMonomial where
  (+) x y = OneVariableMonomial "x" (coefficient x + coefficient y) (power x)
  (-) x y = OneVariableMonomial "x" (coefficient x - coefficient y) (power x)
  (*) x y = OneVariableMonomial "x" (coefficient x * coefficient y) (power x)
  abs x = OneVariableMonomial "x" (abs (coefficient x)) (power x)
  signum x = oneMonomialC (abs (coefficient x))
  fromInteger x = oneMonomialC (fromIntegral x)

x = oneMonomialX (7 / (-3)) 5
y = oneMonomialX 2 5

main = putStrLn $ prettify z ++ "\n" ++ 
  toTex z ++ "\n" ++
  prettify (signum z) 
  where z = x + y



