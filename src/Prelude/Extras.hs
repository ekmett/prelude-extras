{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DefaultSignatures #-}
module Prelude.Extras
  (
  -- * Lifted Prelude classes for kind * -> *
    Eq1(..), eq1, eqWithDefault
  , Ord1(..), compare1, compareWithDefault
  , Show1(..), showsPrec1, showsPrecWithDefault, show1, shows1
  , Read1(..), readsPrec1, read1, reads1, readPrec1
  -- * Lifted Prelude classes for kind * -> * -> *
  , Eq2(..), eq2, eqWith2Default
  , Ord2(..), compare2, compareWith2Default
  , Show2(..), showsPrec2, showsPrecWith2Default, show2, shows2
  , Read2(..), readsPrec2, read2, reads2, readPrec2
  -- * Utilities
  , Equal(..)
  , Compared(..)
  , Shown(..)
  ) where

import Control.Arrow (first)
import Data.Foldable
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Generic
import Data.Traversable
import Text.Read
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.Read.Lex as L

data Equal a = Equal (a -> a -> Bool) a

instance Eq (Equal a) where
  Equal f a == Equal _ b = f a b

class Eq1 f where
  eqWith :: (a -> a -> Bool) -> f a -> f a -> Bool
  default eqWith :: Deciding Eq f => (a -> a -> Bool) -> f a -> f a -> Bool
  eqWith f = getEquivalence $ deciding1 (Equivalence (==)) (Equivalence f)

eqWithDefault :: (Functor f, Eq (f (Equal a)) => (a -> a -> Bool) -> f a -> f a -> Bool
eqWithDefault f as bs = Equal f <$> as == Equal f <$> bs

eq1 :: (Eq1, Eq a) => f a -> f a -> Bool
eq1 = eqWith (==)

instance Eq1 Maybe where
  eqWith f (Just a) (Just b) = f a b
  eqWith _ Nothing Nothing = True
  eqWith _ _ _ = False

instance Eq a => Eq1 (Either a) where
  eqWith f (Right a) (Right b) = f a b
  eqWith f (Left a) (Left b) = a == b
  eqWith _ _ _ = False

instance Eq1 [] where
  eqWith f (a:as) (b:bs) = f a b && eqWith f as bs
  eqWith _ [] [] = True
  eqWith _ _ _ = False

class Eq2 f where
  eqWith2 :: (a -> a -> Bool) -> (b -> b -> Bool) -> f a b -> f a b -> Bool

eqWith2Default :: (Bifunctor f, Eq (f (Equal a) (Equal b))) => (a -> a -> Bool) -> (b -> b -> Bool) -> f a b -> f a b -> Bool
eqWith2Default f g as bs = bimap (Equal f) (Equal g) as == bimap (Equal f) (Equal g) bs

eq2 :: (Eq2, Eq a, Eq b) => f a b -> f a b -> Bool
eq2 = eqWith2 (==) (==)

instance Eq2 Either where
  eqWith2 f _ (Left a) (Left b) = f a b
  eqWith2 _ g (Right a) (Right b) = g a b
  eqWith2 f g _ _ = False

instance Eq2 (,) where
  eqWith2 f g (a,b) (c,d) = f a c && g b d

data Compared a = Compared (a -> a -> Ordering) a 

instance Eq (Compared a) where
  Compared f a `compare` Compared _ b = f a b == EQ

instance Ord (Compared a) where
  Compared f a `compare` Compared _ b = f a b

class Eq1 f => Ord1 f where
  compareWith :: (a -> a -> Ordering) -> f a -> f a -> Ordering
  default compareWith :: Deciding1 Ord f => (a -> a -> Ordering) -> f a -> f a -> Ordering
  compareWith f = getComparison $ deciding1 (Comparison compare) (Comparison f)

compareWithDefault :: (Functor f, Ord (f (Compared a))) => (a -> a -> Ordering) -> f a -> f a -> Ordering
compareWithDefault f as bs = compare (Compared f <$> as) (Compared f <$> bs)

compare1 :: (Ord1 f, Ord a) => f a -> f a -> ordering
compare1 = compareWith compare

instance Ord1 Maybe where
  compareWith f (Just a) (Just b) = f a b
  compareWith _ Nothing Nothing   = EQ
  compareWith _ Just{} Nothing    = LT
  compareWith _ Nothing Just{}    = GT

instance Ord a => Ord1 (Either a) where
  compareWith f (Left a) (Left b) = compare a b
  compareWith f (Right a) (Right b) = f a b
  compareWith _ Left{} Right{} = LT
  compareWith _ Right{} Left{} = GT

instance Ord1 [] where
  compareWith f (a:as) (b:bs) = f a b `mappend` compareWith f as bs
  compareWith _ []     []     = EQ
  compareWith _ (_:_)  []     = GT
  compareWith _ []     (_:_)  = LT

class Eq2 f => Ord2 f where
  compareWith2 :: Ord a => (a -> a -> Ordering) -> (b -> b -> Ordering) -> f a b -> f a b -> Ordering

compareWith2Default :: (Bifunctor f, Ord (f (Compared a) (Compared b))) => (a -> a -> Ordering) -> (b -> b -> Ordering) -> f a b -> f a b -> Ordering
compareWith2Default f g as bs = bimap (Compared f) (Compared g) as `compare` bimap (Compared f) (Compared g) bs

compare2 :: (Ord2 f, Ord a, Ord b) => f a b -> f a b -> Ordering
compare2 = compareWith2 compare compare

data Shown a = Shown (Int -> a -> ShowS) ([a] -> ShowS) a

instance Show (Shown a) where
  showsPrec d (Shown f _ a) = f d a

  showsList [] = showString "[]" -- =(
  showsList xs@(Shown _ _ g) = f [ a | Shown _ a _ <- xs]

class Show1 f where
  showsPrecWith :: Show a => (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> f a -> ShowS
  default showsPrecWith :: (Show2 p, Show b, f ~ p b, Show a) => (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> f a -> ShowS
  showsPrecWith = showsPrecWith2 showsPrec showList

showsPrecWithDefault :: (Functor f, Show (f Shown)) => (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> f a -> ShowS
showsPrecWithDefault f g d as = showsPrec d $ fmap (Shown f g) as

showsPrec1 :: (Show1 f, Show a) => Int -> f a -> ShowS
showsPrec1 = showsPrecWith showsPrec showList

show1 :: (Show1 f, Show a) => f a -> String
show1 x = shows1 x ""

shows1 :: (Show1 f, Show a) => f a -> ShowS
shows1 = showsPrec1 0

instance Show1 Maybe where showsPrecWith = showsPrecWithDefault
  showsPrecWith _ _ _ Nothing  = showString "Nothing"
  showsPrecWith f _ d (Just x) = showParen (d > 10) $ showString "Just " .  f 11 x

instance Show1 [] where 
  showsPrecWith _ g _ [] = g []

instance Show a => Show1 (Either a) where
  showsPrecWith _ _ d (Left a) = showParen (d > 10) $ showString "Left " . showsPrec 11 x
  showsPrecWith f _ d (Right b) = showParen (d > 10) $ showString "Right " . f 11 x

instance Show a => Show1 ((,) a) where
  showsPrecWith f _ (a, b) = show_tuple [shows a, f 0 b]

class Show2 f where
  showsPrecWith2 :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> (Int -> b -> ShowS) -> ([b] -> ShowS) -> Int -> f a b -> ShowS

showsPrecWith2Default :: (Bifunctor f, Show (f (Shown a) (Shown b))) => (Int -> a -> ShowS) -> ShowS [a] -> (Int -> b -> ShowS) -> ShowS [b] -> Int -> f a b -> ShowS
showsPrecWith2Default f g h i d as = showsPrec d $ bimap (shown f g) (shown h i) as

showsPrec2 :: (Show2 f, Show a, Show b) => Int -> f a b -> ShowS
showsPrec2 = showsPrecWith showsPrec showsPrec

show2 :: (Show2 f, Show a, Show b) => f a b -> String
show2 x = shows2 x "" ""

shows2 :: (Show2 f, Show a, Show b) => f a -> ShowS
shows2 = showsPrec2 0

show_tuple :: [ShowS] -> ShowS
show_tuple ss = showChar '('
              . foldr1 (\s r -> s . showChar ',' . r) ss
              . showChar ')'

instance Show2 (,)    where
  showsPrecWith f _ g _ (a, b) = show_tuple [f 0 a, g 0 b]

instance Show2 Either where
  showsPrecWith2 f _ _ _ d (Left a) = showParen (d > 10) $ showString "Left " . f 11 a
  showsPrecWith2 _ _ g _ d (Right b) = showParen (d > 10) $ showString "Right" . g 11 b

class Read1 f where
  readsPrecWith :: Read a => (Int -> ReadS a) -> ReadS [a] -> Int -> ReadS (f a)

readsPrec1 :: Read a => Int -> ReadS (f a)
readsPrec1 = readsPrecWith readsPrec readsList

readPrec1 :: (Read1 f, Read a) => ReadPrec (f a)
readPrec1 = readS_to_Prec readsPrec1

read1  :: (Read1 f, Read a) => String -> f a
read1 s = either error id (readEither1 s)

reads1 :: (Read1 f, Read a) => ReadS (f a)
reads1 = readsPrec1 minPrec

readEither1 :: (Read1 f, Read a) => String -> Either String (f a)
readEither1 s =
  case [ x | (x,"") <- readPrec_to_S read' minPrec s ] of
    [x] -> Right x
    []  -> Left "Prelude.read: no parse"
    _   -> Left "Prelude.read: ambiguous parse"
 where
  read' =
    do x <- readPrec1
       lift P.skipSpaces
       return x

instance Read1 [] where
  readsPrecWith _ g _ = g

instance Read1 Maybe where
  readsPrec1 = readsPrec
  readList1 = readList

instance Read a => Read1 (Either a) where
  readsPrec1 = readsPrec
  readList1 = readList

instance Read a => Read1 ((,) a) where
  readsPrec1 = readsPrec
  readList1 = readList

class Read2 f where
  readsPrecWith2 :: (Int -> ReadS a) -> ReadS [a] -> (Int -> ReadS b) -> ReadS [b] -> Int -> ReadS (f a b)

readsPrec2 :: (Read2 f, Read a, Read b) => Int -> ReadS (f a b)
readsPrec2 = readsPrecWith2 readsPrec readsList readsPrec readsList

readPrec2 :: (Read2 f, Read a, Read b) => ReadPrec (f a b)
readPrec2 = readS_to_Prec readsPrec2

readListPrec2 :: (Read2 f, Read a, Read b) => ReadPrec [f a b]
readListPrec2 = readS_to_Prec (\_ -> readList2)

instance Read2 (,) where
  readsPrec2 = readsPrec
  readList2 = readList

instance Read2 Either where
  readsPrec2 = readsPrec
  readList2 = readList

read2  :: (Read2 f, Read a, Read b) => String -> f a b
read2 s = either error id (readEither2 s)

reads2 :: (Read2 f, Read a, Read b) => ReadS (f a b)
reads2 = readsPrec2 minPrec

readEither2 :: (Read2 f, Read a, Read b) => String -> Either String (f a b)
readEither2 s =
  case [ x | (x,"") <- readPrec_to_S read' minPrec s ] of
    [x] -> Right x
    []  -> Left "Prelude.read: no parse"
    _   -> Left "Prelude.read: ambiguous parse"
 where
  read' =
    do x <- readPrec2
       lift P.skipSpaces
       return x

#ifdef __GLASGOW_HASKELL__
readList2Default :: (Read2 f, Read a, Read b) => ReadS [f a b]
readList2Default = readPrec_to_S readListPrec2 0

readListPrec2Default :: (Read2 f, Read a, Read b) => ReadPrec [f a b]
readListPrec2Default = list readPrec2
#endif

-- annoying to hav to copy these from Text.Read
list :: ReadPrec a -> ReadPrec [a]
-- ^ @(list p)@ parses a list of things parsed by @p@,
-- using the usual square-bracket syntax.
list readx =
  parens
  ( do L.Punc "[" <- lexP
       (listRest False +++ listNext)
  )
 where
  listRest started =
    do L.Punc c <- lexP
       case c of
         "]"           -> return []
         "," | started -> listNext
         _             -> pfail

  listNext =
    do x  <- reset readx
       xs <- listRest True
       return (x:xs)
