{-# LANGUAGE FlexibleInstances #-}

module SimpleClass where

import Data.List

class Foo a where
  foo :: a -> String

instance {-# OVERLAPPABLE #-} Foo a => Foo [a] where
  foo = concat . intersperse ", " . map foo

instance Foo Char where
  foo c = [c]

instance Foo String where
  foo = id

instance Foo Int where
  foo a = ['a']
