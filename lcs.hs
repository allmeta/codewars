import Data.List

lcs :: String -> String -> String
lcs "" _ = ""
lcs _ "" = ""
lcs xs ys = maximum $ map l $ zip (tails xs) (repeat ys)
  where
    l [("",_)] = ""
    l [(_,"")] = ""
    l (((a:as),(b:bs)):xs) = y
      where y=if a==b then a:(l xs) else l (((a:as),bs):xs)
