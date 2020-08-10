module Codewars.Kata.DigPow where

import Data.Char (digitToInt)

digpow :: Integer -> Integer -> Integer
digpow n p = go 1 . fst . foldl (\(acc,b) a-> (acc+a^b,b+1)) (0,p) $ m
  where m = map (toInteger . digitToInt) $ show n
        go k y
          | z>y = -1
          | z==y = k
          | otherwise = go (k+1) y
            where z=n*k
