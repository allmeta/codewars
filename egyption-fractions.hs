odewars.Kata.Decompose where
import Data.Ratio
import GHC.Real


decompose :: String -> String -> [String]
decompose "0" _ = []
decompose n d 
  | mod a b == 0 = [show $ a `div` b]
  | otherwise = map pretty $ checkGreedy [a % b]
  where 
    pretty (x :% y) 
      | y==1 = "1" 
      | otherwise = (show x)++"/"++(show y)
    a=read n
    b=read d
   
   
checkGreedy :: Integral a => [Ratio a] -> [Ratio a]
checkGreedy xs = 
  if x == 1 then xs 
  else checkGreedy $ (init xs) ++ (greedy $ last xs)
  where
    x = numerator $ last xs
  

greedy :: Integral a => Ratio a -> [Ratio a]
greedy (x :% y) = [n1,n2] 
  where
    n1=1 % c
    c= ceiling $ (toRational y) / (toRational x)
    n2=((-y)`mod` x) % (y*c)
