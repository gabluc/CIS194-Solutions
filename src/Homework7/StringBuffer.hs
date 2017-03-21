{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Homework7.StringBuffer where
import Homework7.Buffer
import Data.Monoid

instance Buffer String where
  toString     = id
  fromString   = id
  line n b     = safeIndex n (lines b)
  replaceLine n l b = unlines . uncurry replaceLine' . splitAt n . lines $ b
      where replaceLine' pre [] = pre
            replaceLine' pre (_:ls) = pre ++ l:ls
  numLines     = length . lines
  value        = length . words

safeIndex :: Int -> [a] -> Maybe a
safeIndex n _ | n < 0 = Nothing
safeIndex _ []        = Nothing
safeIndex 0 (x:_)     = Just x
safeIndex n (_:xs)    = safeIndex (n-1) xs