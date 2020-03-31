module Codewars.G964.Rainfall where

import Data.List.Split
import Data.Maybe

mean :: String -> String -> Double
mean t s = case parse t s of
  Nothing -> (-1.0)
  Just x -> calcMean . parseNums $ x
  
calcMean :: [Double] -> Double
calcMean c = (sum c)/(realToFrac $ length c)

parse :: String->String -> Maybe String
parse town string = lookup town . map (cat . splitOn ":") . lines $ string
  where
    cat [a,b]=(a,b)

parseNums :: String->[Double]
parseNums = map (read . last . words) . splitOn ","
  
variance :: String -> String -> Double
variance t s = case parse t s of 
  Nothing-> (-1.0)
  Just x -> calcVar . parseNums $ x
  where
    calcVar c= (sum $ map (\x-> (x-(calcMean c))**2) c)/(realToFrac $ length c)
