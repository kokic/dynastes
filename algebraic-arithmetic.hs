
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Ratio

data OneVariableMonomial = OneVariableMonomial {
  variable :: String, 
  coefficient :: Rational,
  power :: Rational
} deriving (Eq)

occup :: Integer -> Int
occup x = length (show x)

delta :: Bool -> a -> a -> a
delta True  x _ = x
delta False _ y = y

class Prettify a where 
  prettify :: a -> String
  toTex :: a -> String

box n = delta (occup n > 1) ('{' : s ++ "}") s
  where s = show n

instance Prettify Rational where
  prettify x = show p  ++ '/' : show q
    where p = numerator x
          q = denominator x
  
  toTex x = "\\frac" ++ box p ++ box q
    where p = numerator x
          q = denominator x

x = OneVariableMonomial "x" 1 1
main = putStrLn $ toTex (1 / (-3) :: Rational)


