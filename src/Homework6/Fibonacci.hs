module Homework6.Fibonacci where
import Data.List

--Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)


fibs1 :: [Integer]
fibs1 = map fib [0..]



--Exercise 2


fibonacci :: Integer -> Integer
fibonacci n
  | n >= 0 = fst $ fib' n

fib' :: Integer -> (Integer, Integer)
fib' 0 = (0, 1)
fib' n = let (a, b) = fib' (div n 2)
             c      = a * (b * 2 - a)
             d      = a * a + b * b
         in if mod n 2 == 0
            then (c, d)
            else (d, c + d)


fibs2 :: [Integer]
fibs2 = map fibonacci [0..]


--Exercise 3
data Stream a = Cons a (Stream a) 

instance Show a => Show (Stream a) where
  show st = let sh 0 (Cons x xs) = show x ++ ""
                sh n (Cons x xs) = show x ++ ", " ++ sh (n-1) xs
            in  sh 30 st


--Exercise 4

streamRepeat :: a -> Stream a
streamRepeat n = Cons n $ streamRepeat n

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f n = Cons (n) (streamFromSeed f (f n))


--Exercise 5

nats :: Stream Integer
nats = streamFromSeed (\x -> x + 1) 0


{-

THIS SAVED ME: https://mail.haskell.org/pipermail/beginners/2014-February/013160.html
In this situation, if interleaveStreams evaluates its second argument 
before returning any work, it will never be able to return.  Happily, 
the outer Cons of the result does not depend on the second argument.  I 
can fix the issue just by making the second argument be pattern-matched 
lazily (with ~, i.e. only as soon as any uses of the argument in the 
function are evaluated):
-}
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ~(Cons y ys) = Cons x (Cons y (interleaveStreams xs ys)) 


ruler :: Stream Integer
ruler = foldr1 interleaveStreams $ map streamRepeat [0..]
