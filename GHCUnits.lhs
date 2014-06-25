\begin{code}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | This module would be defined in GHC's 'base' library, and is the
-- extent of the definitions built in to the compiler.
module GHCUnits
    ( Unit(..)
    , type (*:)
    , type (/:)
    , type (^:)
    ) where

import GHC.TypeLits

-- | The type of units of measure.  Inside GHC this will include only
-- the 'One' and 'Base' constructors, and use type families (open
-- type-level functions) for the multiplication and inverse.  For the
-- prototype we use constructors in order to get better type inference
-- behaviour, but it is potentially unsound to do so.
data Unit = One | Base Symbol | Unit :*: Unit | Inv Unit

-- type family (u :: Unit) *: (v :: Unit) :: Unit
type u *: v = u :*: v

-- type family (u :: Unit) /: (v :: Unit) :: Unit
type u /: v = u :*: Inv v

type family (u :: Unit) ^: (n :: Nat)  :: Unit

infixl 7 *:, /:, :*:
infixr 8 ^:
\end{code}
