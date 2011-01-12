module Text.Show.Extras where

class Show1 f where 
  showsPrec1 :: Show a => Int -> f a -> ShowS
  show1      :: Show a => f a -> String
  showList1  :: Show a => [f a] -> ShowS
  showsPrec1 _ x s = show1 x ++ s
  show1 x          = shows1 x ""
  showList1 ls   s = showList__ shows1 ls s

shows1 :: (Show1 f, Show a) => f a -> ShowS
shows1 = showsPrec1 0

instance Show1 Maybe where showsPrec1 = showsPrec
instance Show1 [] where showsPrec1 = showsPrec
instance Show a => Show1 (Either a) where showsPrec1 = showsPrec
instance Show a => Show1 ((,) a) where showsPrec1 = showsPrec

-- instance Show1 Complex

class Show2 f where 
  showsPrec2 :: (Show a, Show b) => Int -> f a b -> ShowS
  show2      :: (Show a, Show b) => f a b -> String
  showList2  :: (Show a, Show b) => [f a b] -> ShowS

  showsPrec2 _ x s = show2 x ++ s
  show2 x          = shows2 x ""
  showList2 ls   s = showList__ shows2 ls s

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
