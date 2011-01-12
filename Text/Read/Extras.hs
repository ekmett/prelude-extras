{-# LANGUAGE CPP #-}

module Text.Read.Extras 
  ( Read1(..), read1, reads1
  , Read2(..), read2, reads2
#ifdef __GLASGOW_HASKELL__
  , readList1Default     -- :: (Read1 f, Read a) => ReadS [f a]
  , readListPrec1Default -- :: (Read1 f, Read a) => ReadPrec [f a]
  , readList2Default     -- :: (Read1 f, Read a) => ReadS [f a]
  , readListPrec2Default -- :: (Read1 f, Read a) => ReadPrec [f a]
#endif
  ) where

import Text.Read
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.Read.Lex as L

class Read1 f where
  readsPrec1    :: Read a => Int -> ReadS (f a)
  readList1     :: Read a => ReadS [f a]

  readsPrec1 = readPrec_to_S readPrec1
  readList1  = readPrec_to_S (list readPrec1) 0 

#ifdef __GLASGOW_HASKELL__
  readPrec1     :: Read a => ReadPrec (f a)
  readListPrec1 :: Read a => ReadPrec [f a]

  readPrec1     = readS_to_Prec readsPrec1
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
  readList2     :: (Read a, Read b) => ReadS [f a b]
  readsPrec2    = readPrec_to_S readPrec2
  readList2     = readPrec_to_S (list readPrec2) 0 

#ifdef __GLASGOW_HASKELL__
  readPrec2     :: (Read a, Read b) => ReadPrec (f a b)
  readListPrec2 :: (Read a, Read b) => ReadPrec [f a b]
  readPrec2     = readS_to_Prec readsPrec2
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
