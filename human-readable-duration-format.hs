formatDuration :: Int -> String
formatDuration 0 = "now"
formatDuration n = 
  concat' $
  -- map (\(q,w)-> unwords [show q, w]) $
  map f $
  filter ((/=) 0 . fst) $
  zip ([y,d,h,m,s]) (["year","day","hour","minute","second"])
    where 
      (m', s) = n `divMod` 60
      (h', m) = m' `divMod` 60
      (d', h) = h' `divMod` 24
      (y, d) = d' `divMod` 365
      f (q,w) | q > 1=unwords [show q,w++"s"]
              | otherwise = unwords [show q,w]

concat' :: [String] -> String
concat' [x] = x
concat' [x,c] = x++" and "++c
concat' (x:xs) = (x++", ")++(concat' xs)

