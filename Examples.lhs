\begin{code}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeOperators #-}

-- | This is end-user code, which makes use of the 'UnitsOfMeasure'
-- library to do calculations.
module Examples where

import GHC.Prim (Proxy#, proxy#)
import UnitsOfMeasure
import Equations


-- Initially, base units (measure types) will just be type-level
-- string literals.  Eventually we might want something tidier
-- involving a special extensible kind, but this will do.
type Kg = Base "kg"
type M  = Base "m"
type S  = Base "s"

-- Yuk.  This really needs a special-purpose syntax, like
--     9.808<m/s^2>
gravityOnEarth = 9.808 `unit` (proxy# :: Proxy# (M /: (S *: S)))

-- :t gravityOnEarth
-- gravityOnEarth :: Quantity Double (M /: (S *: S))

heightOfBuilding = 40.0 `unit` (proxy# :: Proxy# M)

speedOfImpact = sqrt' (cast lemma (2.0 *: (gravityOnEarth *: heightOfBuilding)))
  where
    -- I'm explicitly supplying (part of) the proof that the units are
    -- equivalent; in the real implementation this would be the job of
    -- GHC's constraint solver, which will produce a proof (from the
    -- abelian group axioms).
    lemma :: ('One *: ((M /: (S *: S)) *: M)) :==: ((M /: S) *: (M /: S))
    lemma = commutative `trans` identity
            `trans` trustMe (proxy# :: Proxy# ((M /: (S *: S)) *: M))
                            (proxy# :: Proxy# ((M /: S) *: (M /: S)))

myMass = 65.0 `unit` (proxy# :: Proxy# Kg)

type N = Kg *: (M /: (S *: S))

forceOnGround :: Quantity Double N
forceOnGround = myMass *: gravityOnEarth
\end{code}
