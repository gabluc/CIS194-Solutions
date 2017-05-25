module Homework3.Golf where
import Data.List

-- Exercise 1 Hopscotch

skips :: [a] -> [[a]]
skips xs = map (\x -> every x xs) [1 .. length xs]
  where every n xs =
          case drop (n - 1) xs of
            (y:ys) -> y : every n ys
            [] -> []


--Exercise 2 Local maxima

localMaxima :: [Integer] -> [Integer]
localMaxima xs = let a = []
                 in  calc a xs
                       where calc acc [] = acc
                             calc acc [y,z] = acc
                             calc acc (x:y:z:lst)
                               | (y > x) && (y > z) = calc (y:acc) (y:z:lst)
                               | otherwise           = calc acc (y:z:lst)
                    

--Exercise 3 Histogram

histogram :: [Integer] -> String
histogram xs = let grouped = map (\x -> (head x,length x)) (group $ sort xs)
               in buildHisto grouped ++ "==========\n0123456789\n"
                 where buildHisto xs
                         | (sum $ map (\(x,y) -> y) xs) == 0 = ""
                         | otherwise                         = buildHisto desc ++ histoString
                         where flt         = map (\(x,y) -> if y == 0 then -1 else x) xs
                               desc        = map (\(x,y) -> if (y /= 0) then (x,y-1) else (x,y)) xs
                               histoString = ' ' : map (\x -> if x `elem` flt then '*' else ' ') [1..9] ++ "\n"
