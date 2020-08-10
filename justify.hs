justify :: String -> Int -> String
justify text width = init $ unlines $ mergeAll xs width
  where xs=split [] (words text) width

mergeAll xs w = (map (\ws->merge (zip ws (repeat 1)) 0 w (w-(length $ unwords ws))) $ init xs) ++ (last xs)

merge ws i w 0 = res++(fst $ last ws)
  where res=foldl (\acc (b,n)-> acc++(b++(replicate n ' '))) [] (init ws)
merge ws i w v = merge (a++[(c,j+1)]++b) (i+1 `mod` (length ws -1)) w (v-1)
  where (a,((c,j):b)) = splitAt i ws

split :: [[String]]->[String]->Int->[[String]]
split [] (t:ts) w = split [[t]] ts w
split rs [] _ = reverse rs
split (r:rs) (t:ts) w | (length $ unwords xs) > w = split ([t]:r:rs) ts w | otherwise = split (xs:rs) ts w
  where xs = r++[t]
