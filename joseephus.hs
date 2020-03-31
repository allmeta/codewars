josephusSurvivor :: Int -> Int -> Int
josephusSurvivor n k = josephList [1 .. n] k
  
josephList :: [Int] -> Int -> Int
josephList [n] _ = n
josephList n k =
  let c= k `mod` (length n)
      x = (take (c-1) n)++(drop c n)
  in josephList x (c+c)
