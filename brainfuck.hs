{-#LANGUAGE BangPatterns #-}

module Brainfuck (brainfuck) where
import Data.Char
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as H
import Debug.Trace

brainfuck :: String -> String -> String
brainfuck cod inp = eval 0 0 (H.fromList $ zip [0..] $ replicate 5000 0 :: H.HashMap Int Int) 0 []
  where 
    input=map ord inp
    code=V.fromList cod
    jumplist=V.fromList $ zipWith (\i a->if a `elem` "[]" then skip cod i a else 0) [0..] cod
    g=length cod
    eval !i !c !ps !p !out 
      | c>=g = out 
      | otherwise = 
        case code V.! c of
          '>'->eval i (c+1) ps (wrap 5000 $ p+1) out
          '<'->eval i (c+1) ps (wrap 5000 $ p-1) out
          '+'->eval i (c+1) (H.insertWith (\a b->wrap 255 $ a+b) p 1 ps) p out
          '-'->eval i (c+1) (H.insertWith (\a b->wrap 255 $ b-a) p 1 ps) p out
          '.'->eval i (c+1) ps p (out++[chr $ ps H.! p])
          ','->eval (i+1) (c+1) (H.insert p (input!!i) ps) p out
          '['->if ps H.! p == 0 then 
                eval i (jumplist V.! c) ps p out else 
                eval i (c+1) ps p out
          ']'->if ps H.! p /= 0 then
                eval i (jumplist V.! c) ps p out else
                eval i (c+1) ps p out

--update p v ps = a V.++ (V.fromList [v]) V.++ V.tail b
--  where (a,b)=V.splitAt p ps

wrap m (-1)= m-1
wrap m n | n>m-1=0 | otherwise=n

skip code c '[' = foldl f 1 $ filter (flip elem "[]" . snd) $ drop (c+1) $ zip [0..] code where 
  f 1 (a,']') = a + 1
  f i (_,']') = i-1
  f i (_,'[') = i+1
skip code c ']' = foldr f 1 $ filter (flip elem "[]" . snd) $ take c $ zip [0..] code where 
  f (a,'[') 1 = a+1
  f (_,'[') i = i-1
  f (_,']') i = i+1
