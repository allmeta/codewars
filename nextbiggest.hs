import Data.Char
import Data.List
import Data.Ord

nextBigger :: Int -> Int
nextBigger  n = next i s
  where i = check . reverse $ s
        s=zip [0..] $ show n

next :: Int ->[(Int,Char)]->Int
next (-1) _ = -1
next i s = read res
  where (b1,a1)=lol $ splitAt i s
        z = snd $ s!!i
        j = findLarger i a1
        x = snd $ s!!j
        (b2,a2)=lol $ splitAt j a1
        res = (map snd b1)++(sort $ (:) z $ map snd $ b2++a2)
        
findLarger y xs = snd $ minimum $ map (\(a,b)->(b,a)) $ filter (\(_,s)->digitToInt s > y) xs

lol (b,[])=(b,[])
lol (b,(_:a))=(b,a)


check :: [(Int,Char)]->Int
check [_] = -1
check ((i,x):(j,y):xs) | yy<xx=j | otherwise = check $ (j,y):xs
  where xx=digitToInt x
        yy=digitToInt y
