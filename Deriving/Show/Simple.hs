{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Deriving.Show.Simple (showsPrecSimple, WrapSimple(..)) where

import Data.Proxy
import Data.List (intersperse)
import GHC.Generics
import GHC.TypeLits

-- | Like 'showsPrec', but shows it as if their record fields are stripped
showsPrecSimple :: (Generic a, GShowSimple (Rep a)) => Int -> a -> ShowS
showsPrecSimple p a = case shows' (from a) of
  [x] -> x
  xs -> showParen (p > 10) $ foldr (.) id
    $ intersperse (showChar ' ') xs

class GShowSimple f where
  shows' :: f x -> [ShowS]

instance GShowSimple V1 where
  shows' _ = error "impossible"

instance GShowSimple U1 where
  shows' _ = []

instance Show a => GShowSimple (K1 c a) where
  shows' (K1 a) = [showsPrec 11 a]

instance (GShowSimple f, GShowSimple g) => GShowSimple (f :*: g) where
  shows' (f :*: g) = shows' f ++ shows' g

instance (GShowSimple f, GShowSimple g) => GShowSimple (f :+: g) where
  shows' (L1 a) = shows' a
  shows' (R1 a) = shows' a

instance (KnownSymbol name, GShowSimple f, c ~ 'MetaCons name fix rec) => GShowSimple (C1 c f) where
  shows' (M1 a) = showString (symbolVal (Proxy :: Proxy name)) : shows' a

instance (GShowSimple f) => GShowSimple (D1 meta f) where
  shows' (M1 a) = shows' a

instance (GShowSimple f) => GShowSimple (S1 meta f) where
  shows' (M1 a) = shows' a

-- | The 'Show' instance uses 'showsPrecSimple'. Useful in combination with DerivingVia
newtype WrapSimple a = WrapSimple { unwrapSimple :: a }

instance (Generic a, GShowSimple (Rep a)) => Show (WrapSimple a) where
  showsPrec d = showsPrecSimple d . unwrapSimple

