
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

#include "basis.hs"

--- --- --- --- --- --- --- --- --- --- --- --- ---

main = putStrLn $ trans "Stach! Hasse!" italic

--- --- --- --- --- --- --- --- --- --- --- --- ---

lowerCaseOffset = ord 'a' - ord 'Z' - 1

isUpperCase x = ord 'A' <= ord x && ord x <= ord 'Z'
isLowerCase x = ord 'a' <= ord x && ord x <= ord 'z'

class Trans a where 
  trans :: a -> [Char] -> a

instance Trans Char where 
  trans x xs | isUpperCase x = xs !! (ord x - ord 'A')
  trans x xs | isLowerCase x = xs !! (ord x - ord 'A' - lowerCaseOffset)
  trans x _ = x
  
instance Trans 𝕾 where 
  trans (x:[]) xs = [trans x xs] -- trans [] _ = []
  trans (e:es) xs = trans e xs : trans es xs

--- --- --- --- --- --- --- --- --- --- --- --- ---

serifBold = ['𝐀'..'𝐳']

italicRepair :: Char -> Char
italicRepair x | ord x == 119893 = '_' -- miss `h`
italicRepair x = x
italic = map italicRepair ['𝐴'..'𝑧']

italicBold = ['𝑨'..'𝒛']
