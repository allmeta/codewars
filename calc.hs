module Calculator where

evaluate :: String -> Double
evaluate = read . priority . words

priority = op ("+") . op ("-") . op ("*") . op ("/") . parenthesis

parenthesis s | "(" `notElem` s = s | otherwise= parenthesis $ b ++ (priority c) ++ a
  where b = takeWhile((/=) "(") s
        c =  priority $ takeWhile((/=) ")") $ tail $ dropWhile((/=) "(") s
        a =  tail $ dropWhile((/=) ")") s

op f s | f `notElem` s = s | otherwise = op f $ bsr ++ (show $ fs f rsb rsa) ++ asr
  where b = takeWhile((/=)  f) s
        (_:a) =dropWhile((/=) f) s
        rb = last b
        ra = head a
        rsb = read rb :: Double
        rsa = read ra :: Double
        bsr = init b
        asr = tail a

fs "+"  = (+)
fs "-" = (-) 
fs "/"  = (/) 
fs "*" = (*) 
