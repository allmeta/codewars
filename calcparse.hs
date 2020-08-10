module Calculator where

data Calc = Number Double | Operator String 

instance Eq Calc where
  (Operator a) == (Operator b) = a==b
  (Number a) == (Number b) = a==b
  (Operator a) == (Number b) = False

evaluate :: String -> Double
evaluate = get . head . priority . parenthesis . words
  where get (Number n) = n


priority :: [Calc] -> [Calc]
priority = op (Operator "+") . op (Operator "-") . op (Operator "*") . op (Operator "/") 

parse :: String -> Calc
parse "*" = Operator "*"
parse "/" = Operator "/" 
parse "-" = Operator "-"
parse "+" = Operator "+"
parse x = Number (read x)

parenthesis :: [String] -> [Calc]
parenthesis s | "(" `notElem` s = map parse s | otherwise=(parenthesis b) ++ (priority $ map parse c) ++ (parenthesis a)
  where b = takeWhile((/=) "(") s
        c = takeWhile((/=) ")") $ tail $ dropWhile((/=) "(") s
        a = tail $ dropWhile((/=) ")") s

op :: Calc -> [Calc] -> [Calc]
op f s | f `notElem` s = s | otherwise = op f $ b ++ [(op' f ra rb)] ++ a
  where b1 = takeWhile((/=)  f) s
        a1 = tail $ dropWhile((/=) f) s
        ra=head a1
        a=tail a1
        rb=last b1
        b=init b1

op' (Operator f) (Number a) (Number b) = Number (parseOp f b a) 

parseOp "*" = (*) 
parseOp "/" = (/) 
parseOp "-" = (-) 
parseOp "+" = (+) 
