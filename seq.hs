import Data.List
import Data.Char

sequences :: String -> [[Int]]
sequences = traverse f 
  where
    f '?'=[0,1]
    f x = [digitToInt x]

inversions = go 0
  where
  go acc [x] = acc
  go acc (x:xs) = go (acc+n) xs
    where n=if x==1 then length $ filter ((==) 0) xs else 0
    
main = interact (show . solve . readInput)
readInput = head . words
solve = flip mod (10^9+7). sum . map inversions . sequences
