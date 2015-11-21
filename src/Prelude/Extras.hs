{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
#if __GLASGOW_HASKELL__ > 702
#define DEFAULT_SIGNATURES
{-# LANGUAGE DefaultSignatures #-}
#endif
#if __GLASGOW_HASKELL__ >= 701
-- GHC.Conc isn't generally safe, but we're just using TVar
{-# LANGUAGE Trustworthy #-}
#endif

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
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

import Control.Applicative
import Data.Fixed
import Data.IORef (IORef)
import Data.Monoid
#if MIN_VERSION_base(4,4,0)
import Data.Complex (Complex)
import Data.Ratio (Ratio)
import Control.Concurrent (Chan, MVar)
#else
import Control.Concurrent (MVar)
#endif
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.StablePtr (StablePtr)
import GHC.Conc (TVar)
import Text.Read
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.Read.Lex as L

#if MIN_VERSION_base(4,8,0)
import Data.Functor.Identity
#else
import Data.Foldable
import Data.Traversable
#endif

#if MIN_VERSION_base(4,7,0)
import Data.Proxy
#endif

#if MIN_VERSION_base(4,6,0)
import Data.Ord (Down(..))
#endif

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
  (==#) = (==)

instance Eq a => Eq1 (Either a) where
  (==#) = (==)

instance Eq1 [] where
  (==#) = (==)

#if MIN_VERSION_base(4,8,0)
instance Eq1 Identity where (==#) = (==)
deriving instance Eq1 f => Eq1 (Alt f)
#endif
#if MIN_VERSION_base(4,7,0)
instance Eq1 Proxy where (==#) = (==)
instance Eq1 ZipList where (==#) = (==)
#else
instance Eq1 ZipList where ZipList xs ==# ZipList ys = xs == ys
#endif
#if MIN_VERSION_base(4,6,0)
instance Eq1 Down where (==#) = (==)
#endif
#if MIN_VERSION_base(4,8,0)
instance Eq a => Eq1 (Const a) where (==#) = (==)
#else
instance Eq a => Eq1 (Const a) where
  Const a ==# Const b = a == b
#endif
instance Eq1 Dual where (==#) = (==)
instance Eq1 Sum where (==#) = (==)
instance Eq1 Product where (==#) = (==)
instance Eq1 First where (==#) = (==)
instance Eq1 Last where (==#) = (==)
instance Eq1 Ptr where (==#) = (==)
instance Eq1 FunPtr where (==#) = (==)
instance Eq1 MVar where (==#) = (==)
instance Eq1 IORef where (==#) = (==)
instance Eq1 ForeignPtr where (==#) = (==)
instance Eq1 TVar where (==#) = (==)
instance Eq1 Fixed where (==#) = (==)
instance Eq1 StablePtr where (==#) = (==)
#if MIN_VERSION_base(4,4,0)
instance Eq1 Ratio where (==#) = (==)
instance Eq1 Complex where (==#) = (==)
instance Eq1 Chan where (==#) = (==)
#endif

instance Eq a                                                   => Eq1 ((,)         a)                 where (==#) = (==)
instance (Eq a, Eq b)                                           => Eq1 ((,,)        a b)               where (==#) = (==)
instance (Eq a, Eq b, Eq c)                                     => Eq1 ((,,,)       a b c)             where (==#) = (==)
instance (Eq a, Eq b, Eq c, Eq d)                               => Eq1 ((,,,,)      a b c d)           where (==#) = (==)
instance (Eq a, Eq b, Eq c, Eq d, Eq e)                         => Eq1 ((,,,,,)     a b c d e)         where (==#) = (==)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f)                   => Eq1 ((,,,,,,)    a b c d e f)       where (==#) = (==)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g)             => Eq1 ((,,,,,,,)   a b c d e f g)     where (==#) = (==)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h)       => Eq1 ((,,,,,,,,)  a b c d e f g h)   where (==#) = (==)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) => Eq1 ((,,,,,,,,,) a b c d e f g h i) where (==#) = (==)

class Eq2 f where
  (==##) :: (Eq a, Eq b) => f a b -> f a b -> Bool
#ifdef DEFAULT_SIGNATURES
  default (==##) :: (Eq (f a b), Eq a, Eq b) => f a b -> f a b -> Bool
  (==##) = (==)
#endif

(/=##) :: (Eq2 f, Eq a, Eq b) => f a b -> f a b -> Bool
a /=## b = not (a ==## b)

instance Eq2 Either where (==##) = (==)
instance Eq2 Const where (==##) = (==)

instance                                                           Eq2 (,)                              where (==##) = (==)
instance Eq a                                                   => Eq2 ((,,)         a)                 where (==##) = (==)
instance (Eq a, Eq b)                                           => Eq2 ((,,,)        a b)               where (==##) = (==)
instance (Eq a, Eq b, Eq c)                                     => Eq2 ((,,,,)       a b c)             where (==##) = (==)
instance (Eq a, Eq b, Eq c, Eq d)                               => Eq2 ((,,,,,)      a b c d)           where (==##) = (==)
instance (Eq a, Eq b, Eq c, Eq d, Eq e)                         => Eq2 ((,,,,,,)     a b c d e)         where (==##) = (==)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f)                   => Eq2 ((,,,,,,,)    a b c d e f)       where (==##) = (==)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g)             => Eq2 ((,,,,,,,,)   a b c d e f g)     where (==##) = (==)
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h)       => Eq2 ((,,,,,,,,,)  a b c d e f g h)   where (==##) = (==)

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
#if MIN_VERSION_base(4,8,0)
instance Ord1 Identity where compare1 = compare
deriving instance Ord1 f => Ord1 (Alt f)
#endif
#if MIN_VERSION_base(4,7,0)
instance Ord1 Proxy where compare1 = compare
instance Ord1 ZipList where compare1 = compare
#else
instance Ord1 ZipList where compare1 (ZipList xs) (ZipList ys) = compare xs ys
#endif
#if MIN_VERSION_base(4,6,0)
instance Ord1 Down where compare1 = compare
#endif
#if MIN_VERSION_base(4,8,0)
instance Ord a => Ord1 (Const a) where compare1 = compare
#else
instance Ord a => Ord1 (Const a) where
  compare1 (Const x) (Const y) = compare x y
#endif
instance Ord1 Dual where compare1 = compare
instance Ord1 Sum where compare1 = compare
instance Ord1 Product where compare1 = compare
instance Ord1 First where compare1 = compare
instance Ord1 Last where compare1 = compare
instance Ord1 Ptr where compare1 = compare
instance Ord1 FunPtr where compare1 = compare
instance Ord1 ForeignPtr where compare1 = compare
instance Ord1 Fixed where compare1 = compare

instance Ord a                                                           => Ord1 ((,)         a)                 where compare1 = compare
instance (Ord a, Ord b)                                                  => Ord1 ((,,)        a b)               where compare1 = compare
instance (Ord a, Ord b, Ord c)                                           => Ord1 ((,,,)       a b c)             where compare1 = compare
instance (Ord a, Ord b, Ord c, Ord d)                                    => Ord1 ((,,,,)      a b c d)           where compare1 = compare
instance (Ord a, Ord b, Ord c, Ord d, Ord e)                             => Ord1 ((,,,,,)     a b c d e)         where compare1 = compare
instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f)                      => Ord1 ((,,,,,,)    a b c d e f)       where compare1 = compare
instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g)               => Ord1 ((,,,,,,,)   a b c d e f g)     where compare1 = compare
instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h)        => Ord1 ((,,,,,,,,)  a b c d e f g h)   where compare1 = compare
instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i) => Ord1 ((,,,,,,,,,) a b c d e f g h i) where compare1 = compare


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
instance Ord2 Const where compare2 = compare

instance                                                                    Ord2 (,)                              where compare2 = compare
instance Ord a                                                           => Ord2 ((,,)         a)                 where compare2 = compare
instance (Ord a, Ord b)                                                  => Ord2 ((,,,)        a b)               where compare2 = compare
instance (Ord a, Ord b, Ord c)                                           => Ord2 ((,,,,)       a b c)             where compare2 = compare
instance (Ord a, Ord b, Ord c, Ord d)                                    => Ord2 ((,,,,,)      a b c d)           where compare2 = compare
instance (Ord a, Ord b, Ord c, Ord d, Ord e)                             => Ord2 ((,,,,,,)     a b c d e)         where compare2 = compare
instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f)                      => Ord2 ((,,,,,,,)    a b c d e f)       where compare2 = compare
instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g)               => Ord2 ((,,,,,,,,)   a b c d e f g)     where compare2 = compare
instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h)        => Ord2 ((,,,,,,,,,)  a b c d e f g h)   where compare2 = compare

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
#if MIN_VERSION_base(4,8,0)
instance Show1 Identity where showsPrec1 = showsPrec
#endif
#if MIN_VERSION_base(4,7,0)
instance Show1 Proxy where showsPrec1 = showsPrec
instance Show1 ZipList where showsPrec1 = showsPrec
#else
instance Show1 ZipList where
  showsPrec1 p (ZipList xs)
    = showString "ZipList {getZipList = "
    . showList xs
    . showString "}"
#endif
#if MIN_VERSION_base(4,8,0)
instance Show1 Down where showsPrec1 = showsPrec
instance Show1 f => Show1 (Alt f) where
  showsPrec1 p (Alt x)
    = showParen (p > 10)
    $ showString "Alt "
    . showsPrec1 11 x
#endif
#if MIN_VERSION_base(4,8,0)
instance Show a => Show1 (Const a) where showsPrec1 = showsPrec
#else
instance Show a => Show1 (Const a) where
  showsPrec1 p (Const x)
     = showParen (p > 10)
     $ showString "Const "
     . showsPrec 11 x
#endif
instance Show1 Dual where showsPrec1 = showsPrec
instance Show1 Sum where showsPrec1 = showsPrec
instance Show1 Product where showsPrec1 = showsPrec
instance Show1 First where showsPrec1 = showsPrec
instance Show1 Last where showsPrec1 = showsPrec
instance Show1 Ptr where showsPrec1 = showsPrec
instance Show1 FunPtr where showsPrec1 = showsPrec
instance Show1 ForeignPtr where showsPrec1 = showsPrec
#if MIN_VERSION_base(4,4,0)
instance Show1 Complex where showsPrec1 = showsPrec
#endif

instance Show a                                                                   => Show1 ((,)         a)                 where showsPrec1 = showsPrec
instance (Show a, Show b)                                                         => Show1 ((,,)        a b)               where showsPrec1 = showsPrec
instance (Show a, Show b, Show c)                                                 => Show1 ((,,,)       a b c)             where showsPrec1 = showsPrec
instance (Show a, Show b, Show c, Show d)                                         => Show1 ((,,,,)      a b c d)           where showsPrec1 = showsPrec
instance (Show a, Show b, Show c, Show d, Show e)                                 => Show1 ((,,,,,)     a b c d e)         where showsPrec1 = showsPrec
instance (Show a, Show b, Show c, Show d, Show e, Show f)                         => Show1 ((,,,,,,)    a b c d e f)       where showsPrec1 = showsPrec
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g)                 => Show1 ((,,,,,,,)   a b c d e f g)     where showsPrec1 = showsPrec
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h)         => Show1 ((,,,,,,,,)  a b c d e f g h)   where showsPrec1 = showsPrec
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i) => Show1 ((,,,,,,,,,) a b c d e f g h i) where showsPrec1 = showsPrec

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

instance Show2 Either where showsPrec2 = showsPrec
instance Show2 Const where showsPrec2 = showsPrec

instance                                                                             Show2 (,)                              where showsPrec2 = showsPrec
instance Show a                                                                   => Show2 ((,,)         a)                 where showsPrec2 = showsPrec
instance (Show a, Show b)                                                         => Show2 ((,,,)        a b)               where showsPrec2 = showsPrec
instance (Show a, Show b, Show c)                                                 => Show2 ((,,,,)       a b c)             where showsPrec2 = showsPrec
instance (Show a, Show b, Show c, Show d)                                         => Show2 ((,,,,,)      a b c d)           where showsPrec2 = showsPrec
instance (Show a, Show b, Show c, Show d, Show e)                                 => Show2 ((,,,,,,)     a b c d e)         where showsPrec2 = showsPrec
instance (Show a, Show b, Show c, Show d, Show e, Show f)                         => Show2 ((,,,,,,,)    a b c d e f)       where showsPrec2 = showsPrec
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g)                 => Show2 ((,,,,,,,,)   a b c d e f g)     where showsPrec2 = showsPrec
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h)         => Show2 ((,,,,,,,,,)  a b c d e f g h)   where showsPrec2 = showsPrec

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

#if MIN_VERSION_base(4,8,0)
instance Read1 Identity where
  readsPrec1 = readsPrec
  readList1 = readList

instance Read1 f => Read1 (Alt f) where
  readsPrec1 p
    = readParen (p > 10) $ \s ->
      do ("Alt",s1) <- lex s
         (x,s2) <- readsPrec1 11 s1
         return (Alt x, s2)

#endif

#if MIN_VERSION_base(4,7,0)
instance Read1 Proxy where
  readsPrec1 = readsPrec
  readList1 = readList
instance Read1 ZipList where
  readsPrec1 = readsPrec
  readList1 = readList
#else
instance Read1 ZipList where
  readsPrec1 _
    = readParen False $ \s ->
      do ("ZipList"   , s1) <- lex s
         ("{"         , s2) <- lex s1
         ("getZipList", s3) <- lex s2
         ("="         , s4) <- lex s3
         (xs          , s5) <- readList s4
         ("}"         , s6) <- lex s5
         return (ZipList xs, s6)
#endif

#if MIN_VERSION_base(4,7,0)
instance Read1 Down where
  readsPrec1 = readsPrec
  readList1 = readList
#elif MIN_VERSION_base(4,6,0)
instance Read1 Down where
  readsPrec1 p = readParen (p > 10) $ \s ->
    do ("Down",s1) <- lex s
       (x     ,s2) <- readsPrec 11 s1
       return (Down x, s2)
#endif

#if MIN_VERSION_base(4,8,0)
instance Read a => Read1 (Const a) where
  readsPrec1 = readsPrec
  readList1 = readList
#else
instance Read a => Read1 (Const a) where
  readsPrec1 p = readParen (p > 10) $ \s ->
    do ("Const",s1) <- lex s
       (x      ,s2) <- readsPrec 11 s1
       return (Const x, s2)
#endif

instance Read1 Dual where
  readsPrec1 = readsPrec
  readList1 = readList

instance Read1 Sum where
  readsPrec1 = readsPrec
  readList1 = readList

instance Read1 Product where
  readsPrec1 = readsPrec
  readList1 = readList

instance Read1 First where
  readsPrec1 = readsPrec
  readList1 = readList

instance Read1 Last where
  readsPrec1 = readsPrec
  readList1 = readList

#if MIN_VERSION_base(4,4,0)
instance Read1 Complex where
  readsPrec1 = readsPrec
  readList1 = readList
#endif

instance Read a                                                                   => Read1 ((,)         a)                 where readsPrec1 = readsPrec; readList1 = readList
instance (Read a, Read b)                                                         => Read1 ((,,)        a b)               where readsPrec1 = readsPrec; readList1 = readList
instance (Read a, Read b, Read c)                                                 => Read1 ((,,,)       a b c)             where readsPrec1 = readsPrec; readList1 = readList
instance (Read a, Read b, Read c, Read d)                                         => Read1 ((,,,,)      a b c d)           where readsPrec1 = readsPrec; readList1 = readList
instance (Read a, Read b, Read c, Read d, Read e)                                 => Read1 ((,,,,,)     a b c d e)         where readsPrec1 = readsPrec; readList1 = readList
instance (Read a, Read b, Read c, Read d, Read e, Read f)                         => Read1 ((,,,,,,)    a b c d e f)       where readsPrec1 = readsPrec; readList1 = readList
instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g)                 => Read1 ((,,,,,,,)   a b c d e f g)     where readsPrec1 = readsPrec; readList1 = readList
instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h)         => Read1 ((,,,,,,,,)  a b c d e f g h)   where readsPrec1 = readsPrec; readList1 = readList
instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h, Read i) => Read1 ((,,,,,,,,,) a b c d e f g h i) where readsPrec1 = readsPrec; readList1 = readList

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

instance Read2 Either where
  readsPrec2 = readsPrec
  readList2 = readList

instance Read2 Const where
  readsPrec2 = readsPrec
  readList2 = readList

instance                                                                             Read2 (,)                              where readsPrec2 = readsPrec; readList2 = readList
instance Read a                                                                   => Read2 ((,,)         a)                 where readsPrec2 = readsPrec; readList2 = readList
instance (Read a, Read b)                                                         => Read2 ((,,,)        a b)               where readsPrec2 = readsPrec; readList2 = readList
instance (Read a, Read b, Read c)                                                 => Read2 ((,,,,)       a b c)             where readsPrec2 = readsPrec; readList2 = readList
instance (Read a, Read b, Read c, Read d)                                         => Read2 ((,,,,,)      a b c d)           where readsPrec2 = readsPrec; readList2 = readList
instance (Read a, Read b, Read c, Read d, Read e)                                 => Read2 ((,,,,,,)     a b c d e)         where readsPrec2 = readsPrec; readList2 = readList
instance (Read a, Read b, Read c, Read d, Read e, Read f)                         => Read2 ((,,,,,,,)    a b c d e f)       where readsPrec2 = readsPrec; readList2 = readList
instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g)                 => Read2 ((,,,,,,,,)   a b c d e f g)     where readsPrec2 = readsPrec; readList2 = readList
instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h)         => Read2 ((,,,,,,,,,)  a b c d e f g h)   where readsPrec2 = readsPrec; readList2 = readList

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

-- annoying to have to copy these from Text.Read
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

-- If Show1 and Read1 are ever derived by the same mechanism as
-- Show and Read, rather than GND, that will change their behavior
-- here.
newtype Lift1 f a = Lift1 { lower1 :: f a }
  deriving (Functor, Foldable, Traversable, Eq1, Ord1, Show1, Read1)

instance (Eq1 f, Eq a) => Eq (Lift1 f a)       where (==) = (==#)
instance (Ord1 f, Ord a) => Ord (Lift1 f a)    where compare = compare1
instance (Show1 f, Show a) => Show (Lift1 f a) where showsPrec = showsPrec1
instance (Read1 f, Read a) => Read (Lift1 f a) where readsPrec = readsPrec1

newtype Lift2 f a b = Lift2 { lower2 :: f a b }
  deriving (Functor, Foldable, Traversable, Eq2, Ord2, Show2, Read2)

instance (Eq2 f, Eq a)     => Eq1 (Lift2 f a)   where (==#) = (==##)
instance (Ord2 f, Ord a)   => Ord1 (Lift2 f a)  where compare1 = compare2
instance (Show2 f, Show a) => Show1 (Lift2 f a) where showsPrec1 = showsPrec2
instance (Read2 f, Read a) => Read1 (Lift2 f a) where readsPrec1 = readsPrec2

instance (Eq2 f, Eq a, Eq b)       => Eq (Lift2 f a b)   where (==) = (==##)
instance (Ord2 f, Ord a, Ord b)    => Ord (Lift2 f a b)  where compare = compare2
instance (Show2 f, Show a, Show b) => Show (Lift2 f a b) where showsPrec = showsPrec2
instance (Read2 f, Read a, Read b) => Read (Lift2 f a b) where readsPrec = readsPrec2
