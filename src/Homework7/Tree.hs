module Homework7.Tree where
import Data.Monoid

data Tree v a = Leaf v a | Branch v (Tree v a) (Tree v a)

tag :: Monoid v => Tree v a -> v
tag (Leaf _ _) = mempty
tag (Branch _ x y) = tag x <> tag y

branch :: Monoid v => Tree v a -> Tree v a -> Tree v a
branch x y = Branch (tag x <> tag y) x y

search :: Monoid v => (v -> Bool) -> Tree v a -> Maybe a
search p t
  | p $ tag t = Just $ go mempty p t 
  | otherwise = Nothing
  where
  go i p (Leaf _ a) = a
  go i p (Branch _ l r) 
    | p (i <> tag l) = go i p l
    | otherwise      = go (i <> tag l) p r

