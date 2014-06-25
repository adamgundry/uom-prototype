\begin{code}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | This module would not be present in the real implementation.  It
-- defines functions for explicitly proving abelian group equations,
-- in the absence of such functionality in the compiler.
module Equations
    ( (:==:)(Refl)
    , sym
    , trans
    , cong
    , trustMe
    , cast
    , associative
    , commutative
    , identity
    , inverse
    ) where

import GHC.Prim (Proxy#, proxy#)
import UnitsOfMeasure


data x :==: y where
  Refl :: x :==: x

infix 4 :==:

sym :: x :==: y -> y :==: x
sym Refl = Refl

trans :: x :==: y -> y :==: z -> x :==: z
trans Refl Refl = Refl

cong :: f :==: g -> x :==: y -> f x :==: g y
cong Refl Refl = Refl

trustMe :: Proxy# (u :: Unit) -> Proxy# v -> u :==: v
trustMe _ _ = error "trustMe"

associative :: (u :*: (v :*: w)) :==: ((u :*: v) :*: w)
associative = trustMe proxy# proxy#

commutative :: (u :*: v) :==: (v :*: u)
commutative = trustMe proxy# proxy#

identity :: (u :*: One) :==: u
identity = trustMe proxy# proxy#

inverse :: (u :*: Inv u) :==: One
inverse = trustMe proxy# proxy#

cast :: u :==: v -> Quantity a u -> Quantity a v
cast _ (MkQuantity x) = MkQuantity x
\end{code}
