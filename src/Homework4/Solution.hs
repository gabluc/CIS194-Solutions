module Homework4.Solution where
import Data.List
import Data.Char

--Exercise 1: Wholemeal programming

-- 1.
fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldl (\acc x -> if even x then (x-2) * acc else acc) 1 



-- 2.
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)


fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate alg
  where alg x = if even x
                then x `div` 2
                else 3 * x + 1

--Exercise 2: Folding with trees

data BTree a = Leaf | Node Integer (BTree a) a (BTree a) deriving (Show, Eq)

foldTree :: (Ord a) => [a] -> BTree a
foldTree = foldr insert Leaf
  where  insert x Leaf  = (Node 0 Leaf x Leaf)
         insert x p@(Node n left a right) 
           | x > a     = rebalance $ Node (height p) (insert x left) a right
           | x < a     = rebalance $ Node (height p) left a (insert x right)
           | otherwise = p
           where
             height Leaf              = 0
             height (Node _ t1 _ t2)  = 1 + (max (height t1) (height t2))
             rebalance (Node n t1 y t2)
               | abs (sy) < 2         = Node n t1 y t2
               | sy == 2 && st1 /= -1 = rotateright (Node n t1 y t2)
               | sy == 2 && st1 == -1 = rotateright (Node n (rotateleft t1) y t2)
               | sy == -2 && st2 /= 1 = rotateleft (Node n t1 y t2)
               | sy == -2 && st2 == 1 = rotateleft (Node n t1 y (rotateright t2))
               where
                 sy  = slope (Node n t1 y t2)
                 st1 = slope t1
                 st2 = slope t2
                 rotateright (Node n (Node n1 t1 y t2) x t3) = Node (n-1) t1 y (Node (n1-1) t2 x t3)
                 rotateleft  (Node n t1 x (Node n1 t2 y t3)) = Node (n-1) (Node (n1-1) t1 y t2) x t3
                 slope Leaf = 0
                 slope (Node _ t1 _ t2) = (height t1) - (height t2)

--Exercise 3: More folds!

--1.  Implement a function

xor :: [Bool] -> Bool
xor = foldl xr False
  where xr a b
          | (a && b) == True = False
          | otherwise        = a || b

--2.  Implement map as a fold.

map' :: (a->b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

--Exercise 4: Finding primes
--cartProd :: [a] -> [b] -> [(a, b)]
--cartProd xs ys = [(x,y) | x <- xs, y <- ys]


cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = (,) <$> xs <*> ys


sieveSundaram :: Integer -> [Integer]
sieveSundaram n = (2:) . filter (<=n) . map (\x -> 2*x +1 ) $ filter (\x -> not $ x `elem` s) [1..n-1]
                  where composeSieve = filter (<n) . map (\(i,j) -> i + j + 2*i*j)
                        iJ = cartProd [1..n] [1..n]
                        s  = composeSieve iJ

