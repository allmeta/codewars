module Codewars.G964.GapInPrimes where

gap :: Integer -> Integer -> Integer -> Maybe (Integer, Integer)
gap g m n = get $ primes m n
  where get [] = Nothing
        get [_] = Nothing
        get (x:y:xs) | y-x==g=Just (x,y) | otherwise = get (y:xs)

primes a b = filter isPrime [a..b]
  where isPrime s = not $ any ((==) 0 . mod s) [2..floor.sqrt.fromIntegral$s] 
