{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Homework7.Scrabble where 
import Data.Monoid
import Data.Foldable
import Data.Char

newtype Score = Score Int
  deriving (Eq,Ord,Show,Num)

instance Monoid Score where 
  mempty  = Score 0
  mappend = (+)

getScore :: Score -> Int
getScore (Score i) = i

score ::  Char -> Score 
score c 
  | c == 'A' = Score 1
  | c == 'B' = Score 3
  | c == 'C' = Score 3
  | c == 'D' = Score 2
  | c == 'E' = Score 1
  | c == 'F' = Score 4
  | c == 'G' = Score 2
  | c == 'H' = Score 4
  | c == 'I' = Score 1
  | c == 'J' = Score 8
  | c == 'K' = Score 5
  | c == 'L' = Score 1
  | c == 'M' = Score 3
  | c == 'N' = Score 1
  | c == 'O' = Score 1
  | c == 'P' = Score 3
  | c == 'Q' = Score 10
  | c == 'R' = Score 1
  | c == 'S' = Score 1
  | c == 'T' = Score 1
  | c == 'U' = Score 1
  | c == 'V' = Score 4
  | c == 'W' = Score 4
  | c == 'X' = Score 8
  | c == 'Y' = Score 4
  | c == 'Z' = Score 10
  | otherwise = mempty

scoreString :: String -> Score
scoreString = foldl' (+) (Score 0) . map (score . toUpper)

