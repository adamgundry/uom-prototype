\begin{code}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | This is a user library for working with units of measure, defined
-- outside GHC.  Note that it re-exports 'GHCUnits' and supplies
-- unit-safe arithmetic operations.  The exact design of this library
-- is open to experiment.
module UnitsOfMeasure
    ( module GHCUnits
    , Quantity(MkQuantity) -- 'MkQuantity' should not really be exported (allows invariant violation)
    , zero
    , unit
    , (+:)
    , (-:)
    , (*:)
    , (/:)
    , sqrt'
    ) where

import GHC.Prim (Proxy#, proxy#)
import GHC.TypeLits
import GHCUnits

type role Quantity representational nominal
newtype Quantity a (u :: Unit) = MkQuantity { unQuantity :: a }
  deriving (Eq, Ord, Show)

zero :: Num a => Quantity a u
zero = MkQuantity 0

unit :: a -> Proxy# u -> Quantity a u
unit x _ = MkQuantity x

(+:) :: Num a => Quantity a u -> Quantity a u -> Quantity a u
MkQuantity x +: MkQuantity y = MkQuantity (x + y)

(-:) :: Num a => Quantity a u -> Quantity a u -> Quantity a u
MkQuantity x -: MkQuantity y = MkQuantity (x - y)

(*:) :: Num a => Quantity a u -> Quantity a v -> Quantity a (u *: v)
MkQuantity x *: MkQuantity y = MkQuantity (x * y)

(/:) :: Fractional a => Quantity a u -> Quantity a v -> Quantity a (u /: v)
MkQuantity x /: MkQuantity y = MkQuantity (x / y)

infixl 6 +:, -:
infixl 7 *:, /:

sqrt' :: Floating a => Quantity a (u :*: u) -> Quantity a u
sqrt' (MkQuantity x) = MkQuantity (sqrt x)

instance (Num a, u ~ One) => Num (Quantity a u) where
  MkQuantity x + MkQuantity y = MkQuantity (x * y)
  MkQuantity x - MkQuantity y = MkQuantity (x - y)
  MkQuantity x * MkQuantity y = MkQuantity (x * y)
  abs    (MkQuantity x) = MkQuantity (abs x)
  signum (MkQuantity x) = MkQuantity (signum x)
  fromInteger = MkQuantity . fromInteger

instance (Fractional a, u ~ One) => Fractional (Quantity a u) where
  fromRational = MkQuantity . fromRational
  MkQuantity x / MkQuantity y = MkQuantity (x / y)


-- This kind of definition had better be illegal, as (u :*: v) should
-- not be apart from any other unit:
type family Bad (u :: Unit) :: *
type instance Bad One       = Bool
type instance Bad (u :*: v) = Int

-- On the other hand this is okay, though not obviously useful:
type family Good (u :: Unit) :: *
type instance Good (Base "kg") = Bool
type instance Good (Base "m")  = Int
\end{code}
