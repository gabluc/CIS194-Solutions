--{-# OPTIONS_GHC -fno-warn-orphans #-}
module Homework8.Party where 
import Data.Monoid
import Data.Tree
import Homework8.Employee

-- Exercise 1 

glCons :: Employee -> GuestList -> GuestList
glCons e (GL xs fun) = GL (e:xs) (fun + empFun e)

instance Monoid GuestList where 
  mempty  = GL [] 0
  mappend = glAppend
    where glAppend (GL xs f1) (GL ys f2) = GL (xs++ys) (f1+f2)


moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2
  | gl1 > gl2 = gl1
  | gl1 < gl2 = gl2
  | otherwise = gl1

-- Exercise 2

treeFold :: (a -> [b] -> b)-> Tree a -> b
treeFold f (Node x xs) = f x (map (treeFold f) xs)

combineGls :: Employee -> [GuestList] -> GuestList
combineGls e g = e `glCons` subGuestList
  where subGuestList = (foldl (\acc gl -> acc <> gl) mempty g)

-- Exercise 3

nextLevel :: Employee -> [(GuestList,GuestList)] -> (GuestList,GuestList)
nextLevel emp gls = let moreFun'  = uncurry moreFun
                        with    = (glCons emp) . mconcat . map snd $ gls
                        without = mconcat . map (moreFun') $ gls
                    in  (with, without)

-- Exercise 4 

maxFun :: Tree Employee -> GuestList 
maxFun = uncurry moreFun . treeFold nextLevel

-- Exercise 5
partyList :: IO ()
partyList = readFile "./company.txt" >>= (\gst -> putStrLn (show $ maxFun $ read gst))
