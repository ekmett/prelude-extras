{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ > 702
#define DEFAULT_SIGNATURES
{-# LANGUAGE DefaultSignatures #-}
#endif

module Prelude.Extras
  (
  -- * Lifted Prelude classes for kind * -> *
    Eq1(..), (/=#)
  , Ord1(..), (<#), (<=#), (>=#), (>#), max1, min1
  , Show1(..), show1, shows1
  , Read1(..), read1, reads1
#ifdef __GLASGOW_HASKELL__
  , readPrec1            -- :: (Read1 f, Read a) => ReadPrec (f a)
  , readListPrec1        -- :: (Read1 f, Read a) => ReadPrec [f a]
  , readList1Default     -- :: (Read1 f, Read a) => ReadS [f a]
  , readListPrec1Default -- :: (Read1 f, Read a) => ReadPrec [f a]
#endif
  , Lift1(..)
  -- * Lifted Prelude classes for kind * -> * -> *
  , Eq2(..), (/=##)
  , Ord2(..), (<##), (<=##), (>=##), (>##), max2, min2
  , Show2(..), show2, shows2
  , Read2(..), read2, reads2
#ifdef __GLASGOW_HASKELL__
  , readPrec2
  , readListPrec2
  , readList2Default
  , readListPrec2Default
#endif
  , Lift2(..)
  ) where

import Control.Arrow (first)
import Text.Read
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.Read.Lex as L

infixr 4 ==#,  /=#,  <#,  <=#,  >=#,  >#
infixr 4 ==##, /=##, <##, <=##, >=##, >##

class Eq1 f where
  (==#) :: Eq a => f a -> f a -> Bool
#ifdef DEFAULT_SIGNATURES
  default (==#) :: (Eq (f a), Eq a) => f a -> f a -> Bool
  (==#) = (==)
#endif

(/=#) :: (Eq1 f, Eq a) => f a -> f a -> Bool
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
#ifdef DEFAULT_SIGNATURES
  default (==##) :: (Eq (f a b), Eq a, Eq b) => f a b -> f a b -> Bool
  (==##) = (==)
#endif

(/=##) :: (Eq2 f, Eq a, Eq b) => f a b -> f a b -> Bool
a /=## b = not (a ==## b)

instance Eq2 Either where
  (==##) = (==)

class Eq1 f => Ord1 f where
  compare1 :: Ord a => f a -> f a -> Ordering
#ifdef DEFAULT_SIGNATURES
  default compare1 :: (Ord (f a), Ord a) => f a -> f a -> Ordering
  compare1 = compare
#endif


(<#), (<=#), (>=#), (>#) :: (Ord1 f, Ord a) => f a -> f a -> Bool
max1, min1 :: (Ord1 f, Ord a) => f a -> f a -> f a

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
#ifdef DEFAULT_SIGNATURES
  default compare2 :: (Ord (f a b), Ord a, Ord b) => f a b -> f a b -> Ordering
  compare2 = compare
#endif


(<##) :: (Ord2 f, Ord a, Ord b) => f a b -> f a b -> Bool
x <=## y = compare2 x y /= GT
(<=##) :: (Ord2 f, Ord a, Ord b) => f a b -> f a b -> Bool
x <##  y = compare2 x y == LT
(>=##) :: (Ord2 f, Ord a, Ord b) => f a b -> f a b -> Bool
x >=## y = compare2 x y /= LT
(>##) :: (Ord2 f, Ord a, Ord b) => f a b -> f a b -> Bool
x >## y  = compare2 x y == GT

max2 :: (Ord2 f, Ord a, Ord b) => f a b -> f a b -> f a b
max2 x y
  | x >=## y  = x
  | otherwise = y

min2 :: (Ord2 f, Ord a, Ord b) => f a b -> f a b -> f a b
min2 x y
  | x <## y   = x
  | otherwise = y

instance Ord2 Either where compare2 = compare

class Show1 f where
  showsPrec1 :: Show a => Int -> f a -> ShowS
#ifdef DEFAULT_SIGNATURES
  default showsPrec1 :: (Show (f a), Show a) => Int -> f a -> ShowS
  showsPrec1 = showsPrec
#endif
  showList1 :: (Show a) => [f a] -> ShowS
  showList1 ls s = showList__ shows1 ls s

show1 :: (Show1 f, Show a) => f a -> String
show1 x = shows1 x ""


shows1 :: (Show1 f, Show a) => f a -> ShowS
shows1 = showsPrec1 0

instance Show1 Maybe where showsPrec1 = showsPrec
instance Show1 [] where showsPrec1 = showsPrec
instance Show a => Show1 (Either a) where showsPrec1 = showsPrec
instance Show a => Show1 ((,) a) where showsPrec1 = showsPrec

-- instance Show1 Complex

class Show2 f where
  showsPrec2 :: (Show a, Show b) => Int -> f a b -> ShowS
#ifdef DEFAULT_SIGNATURES
  default showsPrec2 :: (Show (f a b), Show a, Show b) => Int -> f a b -> ShowS
  showsPrec2 = showsPrec
#endif
  showList2  :: (Show a, Show b) => [f a b] -> ShowS
  showList2 ls s = showList__ shows2 ls s

show2      :: (Show2 f, Show a, Show b) => f a b -> String
show2 x = shows2 x ""


shows2 :: (Show2 f, Show a, Show b) => f a b -> ShowS
shows2 = showsPrec2 0

instance Show2 (,)    where showsPrec2 = showsPrec
instance Show2 Either where showsPrec2 = showsPrec

showList__ :: (a -> ShowS) ->  [a] -> ShowS
showList__ _     []     s = "[]" ++ s
showList__ showx (x:xs) s = '[' : showx x (showl xs)
  where
    showl []     = ']' : s
    showl (y:ys) = ',' : showx y (showl ys)

class Read1 f where
  readsPrec1    :: Read a => Int -> ReadS (f a)
#ifdef DEFAULT_SIGNATURES
  default readsPrec1 :: (Read (f a), Read a) => Int -> ReadS (f a)
  readsPrec1 = readsPrec
#endif

  readList1 :: (Read a) => ReadS [f a]
  readList1  = readPrec_to_S (list readPrec1) 0

#ifdef __GLASGOW_HASKELL__
readPrec1     :: (Read1 f, Read a) => ReadPrec (f a)
readPrec1     = readS_to_Prec readsPrec1

readListPrec1 :: (Read1 f, Read a) => ReadPrec [f a]
readListPrec1 = readS_to_Prec (\_ -> readList1)
#endif

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

#ifdef __GLASGOW_HASKELL__
readList1Default     :: (Read1 f, Read a) => ReadS [f a]
readList1Default = readPrec_to_S readListPrec1 0

readListPrec1Default :: (Read1 f, Read a) => ReadPrec [f a]
readListPrec1Default = list readPrec1
#endif

instance Read1 [] where
  readsPrec1 = readsPrec
  readList1 = readList

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
  readsPrec2    :: (Read a, Read b) => Int -> ReadS (f a b)
#ifdef DEFAULT_SIGNATURES
  default readsPrec2 :: (Read (f a b), Read a, Read b) => Int -> ReadS (f a b)
  readsPrec2 = readsPrec
#endif
  readList2     :: (Read a, Read b) => ReadS [f a b]
  readList2     = readPrec_to_S (list readPrec2) 0

#ifdef __GLASGOW_HASKELL__
readPrec2     :: (Read2 f, Read a, Read b) => ReadPrec (f a b)
readPrec2     = readS_to_Prec readsPrec2

readListPrec2 :: (Read2 f, Read a, Read b) => ReadPrec [f a b]
readListPrec2 = readS_to_Prec (\_ -> readList2)
#endif

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

newtype Lift1 f a = Lift1 { lower1 :: f a }

instance Eq1 f   => Eq1 (Lift1 f)   where Lift1 a ==# Lift1 b = a ==# b
instance Ord1 f  => Ord1 (Lift1 f)  where Lift1 a `compare1` Lift1 b = compare1 a b
instance Show1 f => Show1 (Lift1 f) where showsPrec1 d (Lift1 a) = showsPrec1 d a
instance Read1 f => Read1 (Lift1 f) where
  readsPrec1 d = map (first Lift1) . readsPrec1 d

instance (Eq1 f, Eq a) => Eq (Lift1 f a)       where Lift1 a == Lift1 b = a ==# b
instance (Ord1 f, Ord a) => Ord (Lift1 f a)    where Lift1 a `compare` Lift1 b = compare1 a b
instance (Show1 f, Show a) => Show (Lift1 f a) where showsPrec d (Lift1 a) = showsPrec1 d a
instance (Read1 f, Read a) => Read (Lift1 f a) where
  readsPrec d = map (first Lift1) . readsPrec1 d

newtype Lift2 f a b = Lift2 { lower2 :: f a b }

instance Eq2 f   => Eq2 (Lift2 f)   where Lift2 a ==## Lift2 b = a ==## b
instance Ord2 f  => Ord2 (Lift2 f)  where Lift2 a `compare2` Lift2 b = compare2 a b
instance Show2 f => Show2 (Lift2 f) where showsPrec2 d (Lift2 a) = showsPrec2 d a
instance Read2 f => Read2 (Lift2 f) where
  readsPrec2 d = map (first Lift2) . readsPrec2 d

instance (Eq2 f, Eq a)     => Eq1 (Lift2 f a)   where Lift2 a ==# Lift2 b = a ==## b
instance (Ord2 f, Ord a)   => Ord1 (Lift2 f a)  where Lift2 a `compare1` Lift2 b = compare2 a b
instance (Show2 f, Show a) => Show1 (Lift2 f a) where showsPrec1 d (Lift2 a) = showsPrec2 d a
instance (Read2 f, Read a) => Read1 (Lift2 f a) where
  readsPrec1 d = map (first Lift2) . readsPrec2 d

instance (Eq2 f, Eq a, Eq b)       => Eq (Lift2 f a b)   where Lift2 a == Lift2 b = a ==## b
instance (Ord2 f, Ord a, Ord b)    => Ord (Lift2 f a b)  where Lift2 a `compare` Lift2 b = compare2 a b
instance (Show2 f, Show a, Show b) => Show (Lift2 f a b) where showsPrec d (Lift2 a) = showsPrec2 d a
instance (Read2 f, Read a, Read b) => Read (Lift2 f a b) where
  readsPrec d = map (first Lift2) . readsPrec2 d
