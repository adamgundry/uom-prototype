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
    ( Quantity(MkQuantity) -- 'MkQuantity' should not really be exported (allows invariant violation)
    , zero
    , unit
    , (+:)
    , (-:)
    , (*:)
    , (/:)
    , sqrt'
    , module GHCUnits
    ) where

import GHC.Prim (Proxy#)
import GHCUnits

-- | A 'Quantity a u' is a value of numeric type @a@, annotated with
-- unit @u@, which is erased at runtime.
newtype Quantity a (u :: Unit) = MkQuantity a
  deriving (Eq, Ord, Show)
type role Quantity representational nominal

-- | To fit into the Haskell numeric typeclass hierarchy, we have to
-- fix the units to be dimensionless, because the types of the
-- arithmetic operators cannot change.  This means that a plain
-- numeric literal can be used as a dimensionless quantity.
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

-- | 'zero' is polymorphic in its units, wheras @0@ is dimensionless
zero :: Num a => Quantity a u
zero = MkQuantity 0

-- | Explicitly tag a value with a unit, using a rather ugly proxy
-- type (ideally we would add special-purpose syntax to avoid this).
unit :: a -> Proxy# u -> Quantity a u
unit x _ = MkQuantity x

-- | Addition for quantities works if the units are equal
(+:) :: Num a => Quantity a u -> Quantity a u -> Quantity a u
MkQuantity x +: MkQuantity y = MkQuantity (x + y)

-- | Subtraction for quantities works if the units are equal
(-:) :: Num a => Quantity a u -> Quantity a u -> Quantity a u
MkQuantity x -: MkQuantity y = MkQuantity (x - y)

-- | Multiplication for quantities multiplies the units
(*:) :: Num a => Quantity a u -> Quantity a v -> Quantity a (u *: v)
MkQuantity x *: MkQuantity y = MkQuantity (x * y)

-- | Division for quantities divides the units
(/:) :: Fractional a => Quantity a u -> Quantity a v -> Quantity a (u /: v)
MkQuantity x /: MkQuantity y = MkQuantity (x / y)

infixl 6 +:, -:
infixl 7 *:, /:

-- | Square root for quantities works only if the unit is a square
sqrt' :: Floating a => Quantity a (u :*: u) -> Quantity a u
sqrt' (MkQuantity x) = MkQuantity (sqrt x)
\end{code}
