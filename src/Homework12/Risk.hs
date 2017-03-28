{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Homework12.Risk where

import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first' :: (a -> b) -> (a, c) -> (b, c)
first' f (a, c) = (f a, c)

instance Random DieValue where
  random           = first' DV . randomR (1,6)
  randomR (low,hi) = first' DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army     = Int
type ArmyType = Char

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving Show

--Exercise 1

-- Add MonadRandom to CIS194.cabal in "build depends:"


--Exercise 2
roll :: Int -> Rand StdGen [Int]
roll n = do
  xs <- sequence $ replicate n die
  return $ unDV <$> reverseSort xs
  where reverseSort = reverse . sort

msr :: Army -> ArmyType -> Int
msr 0 _ = 0
msr n 'a'
  | n <= 3    = n - 1
  | otherwise = 3
msr n 'd'
  | n < 2     = 1
  | otherwise = 2

results :: Battlefield -> [Int] -> [Int] -> Battlefield
results b [] _ = b
results b _ [] = b
results b a d = let r = zipWith (\x y -> if x > y then 1 else 0 ) a d
                    check (i1,i2) s
                      | s == 0    = (succ i1,i2)
                      | otherwise = (i1,succ i2) 
                    (x,y) = foldl check (0,0) r
                in Battlefield (attackers b - x) (defenders b - y)


battle :: Battlefield -> Rand StdGen Battlefield
battle b = do
  let attack = attackers b
      defend = defenders b
  a <- roll $ msr attack 'a'
  d <- roll $ msr defend 'd'
  return $ results b a d

--Exercise 3

invade :: Battlefield -> Rand StdGen Battlefield
invade b = do
  if ( (attackers b > 1) && (defenders b > 0) )
  then do
    i <- battle b 
    invade i
  else
    return b


--Exercise 4 

score :: Battlefield -> Bool
score btl
  | defenders btl == 0 = True
  | attackers btl == 1 = False

successProb :: Battlefield -> Rand StdGen Double 
successProb b = do
  simulations <- sequence $ replicate 1000 $ invade b
  let prob (x,y) btl
        | score btl == True = (succ x,y)
        | otherwise         = (x,succ y)
      (a,b) = foldl prob (0,0) simulations
  return $ a / b