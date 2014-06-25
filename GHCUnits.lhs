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

-- | The type of units of measure.  Inside GHC this will probably
-- include only the 'One' and 'Base' constructors, and use type
-- families (open type-level functions) for the multiplication and
-- inverse.
data Unit = One | Base Symbol | Unit :*: Unit | Inv Unit

type u *: v = u :*: v
type u /: v = u :*: Inv v

-- type family (u :: Unit) *: (v :: Unit) :: Unit
-- type family (u :: Unit) /: (v :: Unit) :: Unit
type family (u :: Unit) ^: (n :: Nat)  :: Unit

infixl 7 *:, /:, :*:
infixr 8 ^:
\end{code}