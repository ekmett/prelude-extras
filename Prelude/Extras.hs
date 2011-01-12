module Prelude.Extras 
  ( Eq1(..), Ord1(..)
  , Eq2(..), Ord2(..)
  , module Text.Show.Extras
  , module Text.Read.Extras
  ) where

import Text.Show.Extras
import Text.Read.Extras

infixr 4 ==#,  /=#,  <#,  <=#,  >=#,  >#
infixr 4 ==##, /=##, <##, <=##, >=##, >##

class Eq1 f where
  (==#) :: Eq a => f a -> f a -> Bool
  (/=#) :: Eq a => f a -> f a -> Bool
  a /=# b = not (a ==# b)

instance Eq1 Maybe where
  Just a  ==# Just b  = a == b
  Nothing ==# Nothing = True
  _       ==# _       = False

instance Eq a => Eq1 (Either a) where
  (==#) = (==)

instance Eq1 [] where
  (==#) = (==)
  
class Eq2 f where
  (==##) :: (Eq a, Eq b) => f a b -> f a b -> Bool
  (/=##) :: (Eq a, Eq b) => f a b -> f a b -> Bool
  a /=## b = not (a ==## b)

instance Eq2 Either where
  (==##) = (==)

class Eq1 f => Ord1 f where 
  compare1 :: Ord a => f a -> f a -> Ordering
  (<#), (<=#), (>=#), (>#) :: Ord a => f a -> f a -> Bool
  max1, min1 :: Ord a => f a -> f a -> f a

  compare1 x y
    | x ==# y   = EQ
    | x <=# y   = LT
    | otherwise = GT

  x <=# y = compare1 x y /= GT
  x <#  y = compare1 x y == LT
  x >=# y = compare1 x y /= LT
  x ># y  = compare1 x y == GT

  max1 x y 
    | x >=# y   = x
    | otherwise = y
  min1 x y
    | x <#  y   = x
    | otherwise = y

instance Ord1 Maybe where compare1 = compare
instance Ord a => Ord1 (Either a) where compare1 = compare
instance Ord1 [] where compare1 = compare

-- needs Haskell 2011
-- instance Ord1 Complex where compare1 = compare

class Eq2 f => Ord2 f where 
  compare2 :: (Ord a, Ord b) => f a b -> f a b -> Ordering
  (<##), (<=##), (>=##), (>##) :: (Ord a, Ord b) => f a b -> f a b -> Bool
  max2, min2 :: (Ord a, Ord b) => f a b -> f a b -> f a b

  compare2 x y
    | x ==## y  = EQ
    | x <=## y  = LT
    | otherwise = GT

  x <=## y = compare2 x y /= GT
  x <##  y = compare2 x y == LT
  x >=## y = compare2 x y /= LT
  x >## y  = compare2 x y == GT

  max2 x y 
    | x >=## y  = x
    | otherwise = y
  min2 x y
    | x <## y   = x
    | otherwise = y

instance Ord2 Either where compare2 = compare
