{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Homework5.Calc where
import qualified Homework5.StackVM as S
import Homework5.ExprT
import Homework5.Parser


--Exercise 1

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b



--Exercise 2
evalStr :: String -> Maybe Integer
evalStr str = case parseExp Lit Add Mul str of
                   Just x -> Just $ eval x
                   Nothing -> Nothing


--Exercise 3
class Expr a where
  add :: a -> a -> a
  mul :: a -> a -> a
  lit :: Integer -> a


instance Expr ExprT where
  lit a = Lit a
  add a b = Add a b
  mul a b = Mul a b

reify :: ExprT -> ExprT
reify = id


--Exercise 4

instance Expr Integer where
  lit a = a
  add a b = a + b
  mul a b = a * b

instance Expr Bool where
  lit n
    | n <= 0 = False
    | n  > 0 = True
  add a b = a || b
  mul a b = a && b


newtype MinMax  = MinMax Integer deriving (Eq, Show,Ord)
newtype Mod7    = Mod7 Integer deriving (Eq, Show,Ord)


instance Expr MinMax where
  lit a   = MinMax a
  add a b = max a b
  mul a b = min a b


instance Num Mod7 where
  (Mod7 a) + (Mod7 b) = Mod7 (a + b)
  (Mod7 a) * (Mod7 b) = Mod7 (a * b)

instance Real Mod7

instance Integral Mod7 where
  (Mod7 x) `mod` (Mod7 y) =  Mod7 $ x `mod` y

instance Enum Mod7 where
  succ (Mod7 a) = Mod7 $ succ a
  pred (Mod7 a) = Mod7 $ succ a

instance Expr Mod7 where
  lit a   = Mod7 a
  add a b = mod (a + b) (Mod7 7)
  mul a b = mod (a * b) (Mod7 7)



testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7
testStack    = testExp :: Maybe S.Program

--Exercise 5

instance Expr S.Program where
  lit a = [S.PushI a]
  add a b = b ++ a ++ [S.Add] 
  mul a b = b ++ a ++ [S.Mul] 


compile :: String -> Maybe S.Program
compile = parseExp lit add mul
