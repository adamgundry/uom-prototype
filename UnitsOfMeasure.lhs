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

module UnitsOfMeasure
    ( Unit(..) -- Constructors shouldn't really be exported
    , Quantity -- N.B. MkQuantity not exported!
    , zero
    , unit
    , type (*:)
    , type (^:)
    , type (/:)
    , (+:)
    , (-:)
    , (*:)
    , (/:)
    , sqrt'

    , (:==:)(Refl)
    , trustMe
    , cast
    , associative
    , commutative
    , identity
    , inverse
    ) where

import GHC.Prim (Proxy#, proxy#)
import GHC.TypeLits
import Unsafe.Coerce


-- Base BaseUnit would be nicer, for a type synonym BaseUnit but makes
-- Unit un-promotable; we also need to get at the strings somehow!
data Unit = Base Symbol | Unit :*: Unit | Inv Unit | One

type family (u :: Unit) *: (v :: Unit) :: Unit
type family (u :: Unit) ^: (i :: Nat ) :: Unit
type family (u :: Unit) /: (v :: Unit) :: Unit


type role Quantity nominal representational
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

infixl 6 +:
infixl 7 *:
infixl 7 /:
infixr 8 ^:

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





data x :==: y where
  Refl :: x :==: x

elimEq :: x :==: y -> ((x ~ y) => a) -> a
elimEq Refl v = v

sym :: x :==: y -> y :==: x
sym Refl = Refl

trans :: x :==: y -> y :==: z -> x :==: z
trans Refl Refl = Refl

trustMe :: Proxy# (u :: Unit) -> Proxy# v -> u :==: v
trustMe _ _ = error "trustMe"

associative :: Proxy# (u :: Unit) -> Proxy# v -> Proxy# w -> (u *: (v *: w)) :==: ((u *: v) *: w)
associative _ _ _ = trustMe proxy# proxy#

commutative :: Proxy# (u :: Unit) -> Proxy# v -> (u *: v) :==: (v *: u)
commutative _ _ = trustMe proxy# proxy#

identity :: Proxy# (u :: Unit) -> (u *: One) :==: u
identity _ = trustMe proxy# proxy#

inverse :: Proxy# (u :: Unit) -> (u *: Inv u) :==: One
inverse _ = trustMe proxy# proxy#

cast :: u :==: v -> Quantity a u -> Quantity a v
cast _ (MkQuantity x) = MkQuantity x






data SUnit u where
  SBase  :: KnownSymbol s => SUnit (Base s)
  (:**:) :: SUnit u -> SUnit v -> SUnit (u :*: v)
  SInv   :: SUnit u -> SUnit (Inv u)

eraseSUnit :: SUnit u -> Unit
eraseSUnit SBase      = Base (error "Symbol")
eraseSUnit (u :**: v) = eraseSUnit u :*: eraseSUnit v
eraseSUnit (SInv u)   = Inv (eraseSUnit u)



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
