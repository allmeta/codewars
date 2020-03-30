module Codewars.G964.Mixin where

import Data.List

mix :: [Char] -> [Char] -> [Char]
mix s1 s2 = diff (filter e a) (filter e b)
    where x1=lex s1
          x2=lex s2
          lex=filter (flip elem ['a'..'z']) 
          a=count $ sort x1
          b=count $ sort x2
          isect=intersect x1 x2 
          e (x,_) = flip elem isect x

count :: [Char]->[(Char,Int)]
count xs = 
  filter (flip (>) 1 . snd) $ 
  nub $ 
  map c xs
    where c x = (x, length $ filter ((==) x) xs)

diff :: [(Char,Int)]->[(Char,Int)]->[Char]
diff [] _ = ""
diff _ [] = ""
diff = 
  intercalate "/" .
  map (\(a,b)->a++":"++b) .
  sortBy (\(a,_) (b,_)-> compare a b) .  
  map conv . 
  zip
    where conv ((a1,b1),(a2,b2))= (num b1 b2, replicate (max b1 b2) a1)
          num a b 
            | a > b = "1"
            | a == b = "="
            | otherwise = "2" 
        
