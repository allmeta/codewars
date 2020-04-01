module LastDigit (lastDigit) where
lastDigit :: [Integer] -> Integer
lastDigit [] = 1
lastDigit xs = foldr1 lastDig xs


lastDig :: Integer -> Integer -> Integer
lastDig _ 0 = 1
lastDig 0 _ = 0
lastDig x n = (l^m) `mod` 10
  where
    l = x `mod` 10
    mn = n `mod` 4
    m | mn == 0 = 4
      | otherwise = mn
