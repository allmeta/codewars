{-#LANGUAGE BangPatterns #-}

mod' = 10^9+7
main = interact ( show . alg 0 0 1 1 0 . head .words)

alg :: Int->Int->Int->Int->Int->String -> Int
alg _ _ _ _  !s [] = s `mod` mod'
alg !o !q !pp !p !s (x:xs) = case x of
    '0'-> if q==0
        then alg o q pp p (s+o) xs
        else alg o q pp p b xs
            where b=s+(o*p)+(q*pp)
    '1'-> alg (o+1) q pp p s xs
    '?'-> alg o (q+1) p (p*2) b xs
        where b=(s*2)+(o*p)+(q*pp)
