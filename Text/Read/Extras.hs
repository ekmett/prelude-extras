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

import Prelude.Extras
