
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Ratio
import Data.List

delta :: Bool -> a -> a -> a
delta True  x _ = x
delta False _ y = y






{--

data OneVariableMonomial = OneVariableMonomial {
  variable :: String,
  coefficient :: Rational,
  power :: Rational
} deriving (Eq)

type One = OneVariableMonomial
type Monomial = [One]
type Polynomial = [Monomial]

varOne :: OneVariableMonomial -> String
varOne = variable
varMon :: Monomial -> String
varMon = varOne . head
varPol :: Polynomial -> String
varPol = varMon . head

data Poly = Poly11 { poly11 :: One }
          | Poly1N { poly1N :: Monomial }
          | PolyNN { polyNN :: Polynomial }

type FnOne a = One -> a
type FnMonomial a = Monomial -> a
type FnPolynomial a = Polynomial -> a
type FnUpTriplet a = (FnOne a, FnMonomial a, FnPolynomial a)
type FnPoly a = Poly -> a

polyBranch :: FnOne p -> FnMonomial p -> FnPolynomial p -> FnPoly p
polyBranch branch11 branch1N branchNN x = case x of
  Poly11 val -> branch11 val
  Poly1N val -> branch1N val
  PolyNN val -> branchNN val

polyBranchTuple :: FnUpTriplet p -> FnPoly p
polyBranchTuple (branch11, branch1N, branchNN) = polyBranch branch11 branch1N branchNN

type Id a = a -> a
type IdOne = Id One
type IdMonomial = Id Monomial
type IdPolynomial = Id Polynomial
type IdPoly = Id Poly

polyIdBranch :: IdOne -> IdMonomial -> IdPolynomial -> IdPoly
polyIdBranch id11 id1N idNN = polyBranch (Poly11 . id11) (Poly1N . id1N) (PolyNN . idNN)

polyToOne :: Poly -> One
polyToOne = polyBranch id head (head . head)

varpoly :: Poly -> String
varpoly = variable . polyToOne

powpoly :: Poly -> Rational
powpoly = power . polyToOne

coepoly :: Poly -> Rational
coepoly = coefficient . polyToOne

type FrUpOne a = FnOne a -> FnMonomial a
type FrUpMonomial a = FnMonomial a -> FnPolynomial a

polyRec :: FnOne a -> FrUpOne a -> FrUpMonomial a -> FnUpTriplet a
polyRec for11 bridge1N bridgeNN = (for11, for1N, forNN)
  where for1N = bridge1N for11
        forNN = bridgeNN for1N

manifold :: ((a -> b) -> b -> a -> b) -> (a -> b) -> [a] -> b
manifold f trans xs = foldl' (f trans) (trans (head xs)) (tail xs)

concatByPlus, concatByTimes :: (a -> String) -> [a] -> String
concatByPlus = manifold (\ trans s t -> s ++ "+" ++ trans t)
concatByTimes = manifold (\ trans s t -> s ++ "*" ++ trans t)

concatByRec :: FnOne String -> FnUpTriplet String
concatByRec for11 = polyRec for11 concatByTimes concatByPlus

sortMonomial :: IdMonomial
sortMonomial = sortBy (\ x y -> compare (power x) (power y))
sortPolynomial = sortBy (\ x y -> compare (varpoly x) (varpoly y))
sortpoly = polyBranch Poly11 (Poly1N . sortMonomial)








bundle :: String -> String -> Integer -> String
bundle left right n = delta (occup n > 1) (left ++ s ++ right) s where s = show n
paren :: Integer -> String
paren = bundle "(" ")"
brace :: Integer -> String
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


prettifyPowerf :: String -> Rational -> String
prettifyPowerf _ 0 = []
prettifyPowerf variable 1 = variable
prettifyPowerf variable power = variable ++ '^' : prettify power

toTexPowerf :: String -> Rational -> String
toTexPowerf _ 0 = []
toTexPowerf variable 1 = variable
toTexPowerf variable power = variable ++ '^' : toTex power

instance Prettify One where
   prettify x = prettify (coefficient x) ++ prettifyPowerf (variable x) (power x)
   toTex x = toTex (coefficient x) ++ toTexPowerf (variable x) (power x)

-- instance Prettify Monomial where
--   prettify xs = foldl' (\ s t -> s ++ prettify t) "" xs
--   toTex xs = foldl' (\ s t -> s ++ toTex t) "" xs

-- instance Prettify Polynomial where
--   prettify xs = foldl' (\ s t -> s ++ prettify t) "" xs
--   toTex xs = foldl' (\ s t -> s ++ toTex t) "" xs


instance Prettify Poly where
  prettify = polyBranch for11 for1N forNN
    where (for11, for1N, forNN) = concatByRec prettify

  toTex = polyBranch for11 for1N forNN
    where (for11, for1N, forNN) = concatByRec toTex

oneMonomial :: String -> Rational -> Rational -> One
oneMonomial = OneVariableMonomial
oneMonomialX :: Rational -> Rational -> One
oneMonomialX = oneMonomial "x"
oneMonomialC :: Rational -> One
oneMonomialC coefficient = oneMonomial "" coefficient 0

inject :: (One -> One) -> Poly -> Poly
inject f = Poly11 . f . polyToOne


additionPoly :: Poly -> Poly -> Poly

-- aX^n + bX^n = (a+b)X^n
additionPoly (Poly11 x) (Poly11 y) | iso = ins
  where (a, b) = (coefficient x, coefficient y)
        (u, v) = (variable x, variable y)
        iso = u == v && power x == power y
        ins = Poly11 (oneMonomial (variable x) (a + b) (power x))

-- Poly1N: aX^n + bX^m, PolyNN: aX^n + bY^n 
additionPoly (Poly11 x) (Poly11 y) = PolyNN [[x], [y]]

additionPoly (Poly1N xs) (Poly11 y) = PolyNN [xs, [y]]
-- | u == v = Poly1N xs
  -- | otherwise = PolyNN [xs, [y]]
  -- where (u, v) = (varMon xs,  varOne y)
        -- maybe = find (\ x -> power x == power y) xs 


additionPoly (PolyNN xs) (Poly11 y) = PolyNN ([y] : xs)
additionPoly (Poly11 x) (PolyNN xs) = additionPoly (PolyNN xs) (Poly11 x)

coeffMorphPoly :: (Rational -> Rational) -> Poly
coeffMorphPoly f = inject morph x
  where value = f . coefficient
        morpf = \ one -> oneMonomial (variable one) (value one)
        morph = \ one -> morpf one (power one)

instance Num Poly where
  (+) = additionPoly
    -- case (x, y) of (Poly11 x, Poly11 y) -> 
    -- Poly11 (oneMonomial (variable (ovm x)) (coefficient x - coefficient y) (power x))

  (-) x y = x
  -- (-) x y | eqPowerF x y = oneMonomial (variable x) (coefficient x - coefficient y) (power x)
  -- (*) x y | eqPowerF x y = oneMonomial (variable x) (coefficient x * coefficient y) (power x + power y)
  (*) x y = x

  abs x = coeffMorphPoly abs
  negate x = coeffMorphPoly negate
  signum x = inject (oneMonomialC . signum . coefficient) x
  fromInteger x = (Poly11 . oneMonomialC . fromIntegral) x




x = Poly11 (oneMonomialX (7 / (-3)) 5)
y = Poly11 (oneMonomialX 2 5)

main = putStrLn $ prettify z ++ "\n" ++
  -- toTex z ++ "\n" ++
  prettify (abs z) ++ "\n" ++
  prettify (Poly1N [poly11 x, poly11 y])
  where z = x + y

--}




