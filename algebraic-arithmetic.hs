
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

occup :: Integer -> Int
occup x = length (show x)

class Prettify a where 
  prettify :: a -> String
  toTex :: a -> String

box n = delta (occup n > 1) ('{' : s ++ "}") s
  where s = show n

instance Prettify Rational where
  prettify x = show p  ++ '/' : show q
    where (p, q) = (numerator x, denominator x)
    
  toTex x = "\\frac" ++ box p ++ box q
    where (p, q) = (numerator x, denominator x)



x = OneVariableMonomial "x" 2 3

main = putStrLn $ toTex (7 / (-3) :: Rational)


