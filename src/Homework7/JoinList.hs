{-# LANGUAGE FlexibleInstances #-}
module Homework7.JoinList where 
import Data.Monoid
import Data.Maybe
import Homework7.Sized
import Homework7.Buffer
import Homework7.Editor
import Homework7.Scrabble

data JoinList m a = Empty | Single m a | Append m (JoinList m a) (JoinList m a) deriving (Eq,Show)

single :: a -> JoinList Size a 
single a = Single (Size 1) a 


jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

--Exercise 1
tag :: Monoid m => JoinList m a -> m 
tag Empty = mempty
tag (Single m _ ) = m 
tag (Append _ x y) = tag x <> tag y

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
a +++ b = Append (tag a <> tag b) a b


--Exercise 2
(!?) :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
n !? Empty    = Nothing
n !? (Single _ a) 
  | n == 0    = Just a 
  | otherwise = Nothing
n !? (Append m l r) 
  | n < 0 || n > (getSize . size $ m) = Nothing
  | n < (getSize . size $ tag l)      = n !? l 
  | otherwise                         = (n - (getSize . size $ tag l)) !? r


dropJ :: (Sized b, Monoid b) => Int ->  JoinList b a -> JoinList b a
dropJ n s@(Single _ _)
  | n <= 0 = s
dropJ n a@(Append m l r)
  | n >= (getSize $ size m) = Empty
  | n >=  sizel             = dropJ (n - sizel) r
  | n > 0                   = dropJ n l +++ r
  | otherwise               = a
  where sizel = getSize . size $ tag l

takeJ :: (Sized b, Monoid b) => Int ->  JoinList b a -> JoinList b a
takeJ n s@(Single _ _)
  | n > 0 = s
takeJ n a@(Append m l r)
  | n >= (getSize $ size m) = a 
  | n >  sizel              = l +++ takeJ (n - sizel) r
  | n > 0                   = takeJ n l
  where sizel = getSize . size $ tag l


--Exercise 3
scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

scoreAndSize :: String -> JoinList (Score,Size) String
scoreAndSize str = Single (scoreString str, 1) str

--Exercise 4
instance Buffer (JoinList (Score, Size) String) where
  toString               = unlines . jlToList
  fromString             = foldl (\jl str -> jl +++ scoreAndSize str) Empty . lines
  line                   = (!?)
  replaceLine n str jl   = takeJ n jl +++ fromString str +++ dropJ (n+1) jl
  numLines               = getSize  . snd . tag
  value                  = getScore . fst . tag

testImplementation :: IO()
testImplementation = runEditor editor (fromString "test" :: (JoinList (Score, Size) String))

