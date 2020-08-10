module PolydivisibleNumbers (isPolydivisible,getPolydivisible) where
import Data.List
import Data.Maybe

type ArbitraryBaseDigit = Char
type ArbitraryBaseNumber = [ArbitraryBaseDigit]
type Base = Int
type Index = Int

isPolydivisible :: ArbitraryBaseNumber -> Base -> Bool
isPolydivisible ns b = all (\(p,n)->n`rem`p==0) ms
  where ms = zip [1..] $ map read $ tail $ inits $ show $ convert ns b
          
getPolydivisible :: Index -> Base -> ArbitraryBaseNumber
getPolydivisible = undefined

bases = zip (['0'..'9']++['A'..'Z']++['a'..'z']) [0..]
convert ns b = fst . foldr (\n (acc,i)->(acc+n*b^i,i+1)) (0,0) . map (fromJust . flip lookup bases) $ ns
